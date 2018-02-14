/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Computed values for font properties

use Atom;
use app_units::Au;
use byteorder::{BigEndian, ByteOrder};
use cssparser::Parser;
#[cfg(feature = "gecko")]
use gecko_bindings::{bindings, structs};
#[cfg(feature = "gecko")]
use gecko_bindings::sugar::refptr::RefPtr;
#[cfg(feature = "gecko")]
use malloc_size_of::{MallocSizeOf, MallocSizeOfOps};
use std::fmt::{self, Write};
#[cfg(feature = "gecko")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "servo")]
use std::slice;
use style_traits::{CssWriter, ParseError, ToCss};
use values::CSSFloat;
use values::animated::{ToAnimatedValue, ToAnimatedZero};
use values::computed::{Context, NonNegativeLength, ToComputedValue, Integer, Number};
use values::generics::font::{FamilyName, FamilyNameSyntax, FeatureTagValue, FontSettings};
use values::generics::font::{KeywordInfo as GenericKeywordInfo, VariationValue};
use values::specified::font as specified;

pub use values::computed::Length as MozScriptMinSize;
pub use values::specified::font::{XTextZoom, XLang, MozScriptSizeMultiplier, FontSynthesis};

/// As of CSS Fonts Module Level 3, only the following values are
/// valid: 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900
///
/// However, system fonts may provide other values. Pango
/// may provide 350, 380, and 1000 (on top of the existing values), for example.
#[derive(Clone, ComputeSquaredDistance, Copy, Debug, Eq, Hash, MallocSizeOf, PartialEq, ToCss)]
#[cfg_attr(feature = "servo", derive(Deserialize, Serialize))]
pub struct FontWeight(pub u16);

#[derive(Animate, ComputeSquaredDistance, MallocSizeOf, ToAnimatedZero)]
#[derive(Clone, Copy, Debug, PartialEq)]
/// The computed value of font-size
pub struct FontSize {
    /// The size.
    pub size: NonNegativeLength,
    /// If derived from a keyword, the keyword and additional transformations applied to it
    pub keyword_info: Option<KeywordInfo>,
}

/// Additional information for computed keyword-derived font sizes.
pub type KeywordInfo = GenericKeywordInfo<NonNegativeLength>;

impl FontWeight {
    /// Value for normal
    pub fn normal() -> Self {
        FontWeight(400)
    }

    /// Value for bold
    pub fn bold() -> Self {
        FontWeight(700)
    }

    /// Convert from an integer to Weight
    pub fn from_int(n: i32) -> Result<Self, ()> {
        if n >= 100 && n <= 900 && n % 100 == 0 {
            Ok(FontWeight(n as u16))
        } else {
            Err(())
        }
    }

    /// Convert from an Gecko weight
    pub fn from_gecko_weight(weight: u16) -> Self {
        // we allow a wider range of weights than is parseable
        // because system fonts may provide custom values
        FontWeight(weight)
    }

    /// Weither this weight is bold
    pub fn is_bold(&self) -> bool {
        self.0 > 500
    }

    /// Return the bolder weight
    pub fn bolder(self) -> Self {
        if self.0 < 400 {
            FontWeight(400)
        } else if self.0 < 600 {
            FontWeight(700)
        } else {
            FontWeight(900)
        }
    }

    /// Returns the lighter weight
    pub fn lighter(self) -> Self {
        if self.0 < 600 {
            FontWeight(100)
        } else if self.0 < 800 {
            FontWeight(400)
        } else {
            FontWeight(700)
        }
    }
}

impl FontSize {
    /// The actual computed font size.
    pub fn size(self) -> Au {
        self.size.into()
    }

    #[inline]
    /// Get default value of font size.
    pub fn medium() -> Self {
        Self {
            size: Au::from_px(specified::FONT_MEDIUM_PX).into(),
            keyword_info: Some(KeywordInfo::medium())
        }
    }

    /// FIXME(emilio): This is very complex. Also, it should move to
    /// StyleBuilder.
    pub fn cascade_inherit_font_size(context: &mut Context) {
        // If inheriting, we must recompute font-size in case of language
        // changes using the font_size_keyword. We also need to do this to
        // handle mathml scriptlevel changes
        let kw_inherited_size = context.builder.get_parent_font()
                                       .clone_font_size()
                                       .keyword_info.map(|info| {
            specified::FontSize::Keyword(info).to_computed_value(context).size
        });
        let mut font = context.builder.take_font();
        font.inherit_font_size_from(context.builder.get_parent_font(),
                                    kw_inherited_size,
                                    context.builder.device);
        context.builder.put_font(font);
    }

    /// Cascade the initial value for the `font-size` property.
    ///
    /// FIXME(emilio): This is the only function that is outside of the
    /// `StyleBuilder`, and should really move inside!
    ///
    /// Can we move the font stuff there?
    pub fn cascade_initial_font_size(context: &mut Context) {
        // font-size's default ("medium") does not always
        // compute to the same value and depends on the font
        let computed = specified::FontSize::medium().to_computed_value(context);
        context.builder.mutate_font().set_font_size(computed);
        #[cfg(feature = "gecko")] {
            let device = context.builder.device;
            context.builder.mutate_font().fixup_font_min_size(device);
        }
    }
}

impl ToCss for FontSize {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result where W: fmt::Write {
        self.size.to_css(dest)
    }
}

/// XXXManishearth it might be better to
/// animate this as computed, however this complicates
/// clamping and might not be the right thing to do.
/// We should figure it out.
impl ToAnimatedValue for FontSize {
    type AnimatedValue = NonNegativeLength;

    #[inline]
    fn to_animated_value(self) -> Self::AnimatedValue {
        self.size
    }

    #[inline]
    fn from_animated_value(animated: Self::AnimatedValue) -> Self {
        FontSize {
            size: animated.clamp(),
            keyword_info: None,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "servo", derive(MallocSizeOf))]
/// Specifies a prioritized list of font family names or generic family names.
pub struct FontFamily(pub FontFamilyList);

impl FontFamily {
    #[inline]
    /// Get default font family as `serif` which is a generic font-family
    pub fn serif() -> Self {
        FontFamily(
            FontFamilyList::new(Box::new([SingleFontFamily::Generic(atom!("serif"))]))
        )
    }
}

#[cfg(feature = "gecko")]
impl MallocSizeOf for FontFamily {
    fn size_of(&self, _ops: &mut MallocSizeOfOps) -> usize {
        // SharedFontList objects are generally shared from the pointer
        // stored in the specified value. So only count this if the
        // SharedFontList is unshared.
        unsafe {
            bindings::Gecko_SharedFontList_SizeOfIncludingThisIfUnshared(
                (self.0).0.get()
            )
        }
    }
}

impl ToCss for FontFamily {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result where W: fmt::Write {
        let mut iter = self.0.iter();
        iter.next().unwrap().to_css(dest)?;
        for family in iter {
            dest.write_str(", ")?;
            family.to_css(dest)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, MallocSizeOf, PartialEq)]
#[cfg_attr(feature = "servo", derive(Deserialize, Serialize))]
/// A set of faces that vary in weight, width or slope.
pub enum SingleFontFamily {
    /// The name of a font family of choice.
    FamilyName(FamilyName),
    /// Generic family name.
    Generic(Atom),
}

impl SingleFontFamily {
    #[inline]
    /// Get font family name as Atom
    pub fn atom(&self) -> &Atom {
        match *self {
            SingleFontFamily::FamilyName(ref family_name) => &family_name.name,
            SingleFontFamily::Generic(ref name) => name,
        }
    }

    #[inline]
    #[cfg(not(feature = "gecko"))] // Gecko can't borrow atoms as UTF-8.
    /// Get font family name
    pub fn name(&self) -> &str {
        self.atom()
    }

    #[cfg(not(feature = "gecko"))] // Gecko can't borrow atoms as UTF-8.
    /// Get the corresponding font-family with Atom
    pub fn from_atom(input: Atom) -> SingleFontFamily {
        match input {
            atom!("serif") |
            atom!("sans-serif") |
            atom!("cursive") |
            atom!("fantasy") |
            atom!("monospace") => {
                return SingleFontFamily::Generic(input)
            }
            _ => {}
        }
        match_ignore_ascii_case! { &input,
            "serif" => return SingleFontFamily::Generic(atom!("serif")),
            "sans-serif" => return SingleFontFamily::Generic(atom!("sans-serif")),
            "cursive" => return SingleFontFamily::Generic(atom!("cursive")),
            "fantasy" => return SingleFontFamily::Generic(atom!("fantasy")),
            "monospace" => return SingleFontFamily::Generic(atom!("monospace")),
            _ => {}
        }

        // We don't know if it's quoted or not. So we set it to
        // quoted by default.
        SingleFontFamily::FamilyName(FamilyName {
            name: input,
            syntax: FamilyNameSyntax::Quoted,
        })
    }

    /// Parse a font-family value
    pub fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i>> {
        if let Ok(value) = input.try(|i| i.expect_string_cloned()) {
            return Ok(SingleFontFamily::FamilyName(FamilyName {
                name: Atom::from(&*value),
                syntax: FamilyNameSyntax::Quoted,
            }))
        }
        let first_ident = input.expect_ident()?.clone();

        // FIXME(bholley): The fast thing to do here would be to look up the
        // string (as lowercase) in the static atoms table. We don't have an
        // API to do that yet though, so we do the simple thing for now.
        let mut css_wide_keyword = false;
        match_ignore_ascii_case! { &first_ident,
            "serif" => return Ok(SingleFontFamily::Generic(atom!("serif"))),
            "sans-serif" => return Ok(SingleFontFamily::Generic(atom!("sans-serif"))),
            "cursive" => return Ok(SingleFontFamily::Generic(atom!("cursive"))),
            "fantasy" => return Ok(SingleFontFamily::Generic(atom!("fantasy"))),
            "monospace" => return Ok(SingleFontFamily::Generic(atom!("monospace"))),

            #[cfg(feature = "gecko")]
            "-moz-fixed" => return Ok(SingleFontFamily::Generic(atom!("-moz-fixed"))),

            // https://drafts.csswg.org/css-fonts/#propdef-font-family
            // "Font family names that happen to be the same as a keyword value
            //  (`inherit`, `serif`, `sans-serif`, `monospace`, `fantasy`, and `cursive`)
            //  must be quoted to prevent confusion with the keywords with the same names.
            //  The keywords ‘initial’ and ‘default’ are reserved for future use
            //  and must also be quoted when used as font names.
            //  UAs must not consider these keywords as matching the <family-name> type."
            "inherit" => css_wide_keyword = true,
            "initial" => css_wide_keyword = true,
            "unset" => css_wide_keyword = true,
            "default" => css_wide_keyword = true,
            _ => {}
        }

        let mut value = first_ident.as_ref().to_owned();

        // These keywords are not allowed by themselves.
        // The only way this value can be valid with with another keyword.
        if css_wide_keyword {
            let ident = input.expect_ident()?;
            value.push(' ');
            value.push_str(&ident);
        }
        while let Ok(ident) = input.try(|i| i.expect_ident_cloned()) {
            value.push(' ');
            value.push_str(&ident);
        }
        let syntax = if value.starts_with(' ') || value.ends_with(' ') || value.contains("  ") {
            // For font family names which contains special white spaces, e.g.
            // `font-family: \ a\ \ b\ \ c\ ;`, it is tricky to serialize them
            // as identifiers correctly. Just mark them quoted so we don't need
            // to worry about them in serialization code.
            FamilyNameSyntax::Quoted
        } else {
            FamilyNameSyntax::Identifiers
        };
        Ok(SingleFontFamily::FamilyName(FamilyName {
            name: Atom::from(value),
            syntax
        }))
    }

    #[cfg(feature = "gecko")]
    /// Return the generic ID for a given generic font name
    pub fn generic(name: &Atom) -> (structs::FontFamilyType, u8) {
        use gecko_bindings::structs::FontFamilyType;
        if *name == atom!("serif") {
            (FontFamilyType::eFamily_serif,
             structs::kGenericFont_serif)
        } else if *name == atom!("sans-serif") {
            (FontFamilyType::eFamily_sans_serif,
             structs::kGenericFont_sans_serif)
        } else if *name == atom!("cursive") {
            (FontFamilyType::eFamily_cursive,
             structs::kGenericFont_cursive)
        } else if *name == atom!("fantasy") {
            (FontFamilyType::eFamily_fantasy,
             structs::kGenericFont_fantasy)
        } else if *name == atom!("monospace") {
            (FontFamilyType::eFamily_monospace,
             structs::kGenericFont_monospace)
        } else if *name == atom!("-moz-fixed") {
            (FontFamilyType::eFamily_moz_fixed,
             structs::kGenericFont_moz_fixed)
        } else {
            panic!("Unknown generic {}", name);
        }
    }

    #[cfg(feature = "gecko")]
    /// Get the corresponding font-family with family name
    fn from_font_family_name(family: &structs::FontFamilyName) -> SingleFontFamily {
        use gecko_bindings::structs::FontFamilyType;

        match family.mType {
            FontFamilyType::eFamily_sans_serif => SingleFontFamily::Generic(atom!("sans-serif")),
            FontFamilyType::eFamily_serif => SingleFontFamily::Generic(atom!("serif")),
            FontFamilyType::eFamily_monospace => SingleFontFamily::Generic(atom!("monospace")),
            FontFamilyType::eFamily_cursive => SingleFontFamily::Generic(atom!("cursive")),
            FontFamilyType::eFamily_fantasy => SingleFontFamily::Generic(atom!("fantasy")),
            FontFamilyType::eFamily_moz_fixed => SingleFontFamily::Generic(Atom::from("-moz-fixed")),
            FontFamilyType::eFamily_named => {
                let name = Atom::from(&*family.mName);
                SingleFontFamily::FamilyName(FamilyName {
                    name,
                    syntax: FamilyNameSyntax::Identifiers,
                })
            },
            FontFamilyType::eFamily_named_quoted => SingleFontFamily::FamilyName(FamilyName {
                name: (&*family.mName).into(),
                syntax: FamilyNameSyntax::Quoted,
            }),
            _ => panic!("Found unexpected font FontFamilyType"),
        }
    }
}

impl ToCss for SingleFontFamily {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result where W: fmt::Write {
        match *self {
            SingleFontFamily::FamilyName(ref name) => name.to_css(dest),

            // All generic values accepted by the parser are known to not require escaping.
            SingleFontFamily::Generic(ref name) => {
                #[cfg(feature = "gecko")] {
                    // We should treat -moz-fixed as monospace
                    if name == &atom!("-moz-fixed") {
                        return dest.write_str("monospace");
                    }
                }

                write!(dest, "{}", name)
            },
        }
    }
}

#[cfg(feature = "servo")]
#[derive(Clone, Debug, Eq, Hash, MallocSizeOf, PartialEq)]
/// A list of SingleFontFamily
pub struct FontFamilyList(Box<[SingleFontFamily]>);

#[cfg(feature = "gecko")]
#[derive(Clone, Debug)]
/// A list of SingleFontFamily
pub struct FontFamilyList(pub RefPtr<structs::SharedFontList>);

#[cfg(feature = "gecko")]
impl Hash for FontFamilyList {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        for name in self.0.mNames.iter() {
            name.mType.hash(state);
            name.mName.hash(state);
        }
    }
}

#[cfg(feature = "gecko")]
impl PartialEq for FontFamilyList {
    fn eq(&self, other: &FontFamilyList) -> bool {
        if self.0.mNames.len() != other.0.mNames.len() {
            return false;
        }
        for (a, b) in self.0.mNames.iter().zip(other.0.mNames.iter()) {
            if a.mType != b.mType || &*a.mName != &*b.mName {
                return false;
            }
        }
        true
    }
}

#[cfg(feature = "gecko")]
impl Eq for FontFamilyList {}

impl FontFamilyList {
    #[cfg(feature = "servo")]
    /// Return FontFamilyList with a vector of SingleFontFamily
    pub fn new(families: Box<[SingleFontFamily]>) -> FontFamilyList {
        FontFamilyList(families)
    }

    #[cfg(feature = "gecko")]
    /// Return FontFamilyList with a vector of SingleFontFamily
    pub fn new(families: Box<[SingleFontFamily]>) -> FontFamilyList {
        let fontlist;
        let names;
        unsafe {
            fontlist = bindings::Gecko_SharedFontList_Create();
            names = &mut (*fontlist).mNames;
            names.ensure_capacity(families.len());
        };

        for family in families.iter() {
            match *family {
                SingleFontFamily::FamilyName(ref f) => {
                    let quoted = matches!(f.syntax, FamilyNameSyntax::Quoted);
                    unsafe {
                        bindings::Gecko_nsTArray_FontFamilyName_AppendNamed(
                            names,
                            f.name.as_ptr(),
                            quoted
                        );
                    }
                }
                SingleFontFamily::Generic(ref name) => {
                    let (family_type, _generic) = SingleFontFamily::generic(name);
                    unsafe {
                        bindings::Gecko_nsTArray_FontFamilyName_AppendGeneric(
                            names,
                            family_type
                        );
                    }
                }
            }
        }

        FontFamilyList(unsafe { RefPtr::from_addrefed(fontlist) })
    }

    #[cfg(feature = "servo")]
    /// Return iterator of SingleFontFamily
    pub fn iter(&self) -> slice::Iter<SingleFontFamily> {
        self.0.iter()
    }

    #[cfg(feature = "gecko")]
    /// Return iterator of SingleFontFamily
    pub fn iter(&self) -> FontFamilyNameIter {
        FontFamilyNameIter {
            names: &self.0.mNames,
            cur: 0,
        }
    }

    #[cfg(feature = "gecko")]
    /// Return the generic ID if it is a single generic font
    pub fn single_generic(&self) -> Option<u8> {
        let mut iter = self.iter();
        if let Some(SingleFontFamily::Generic(ref name)) = iter.next() {
            if iter.next().is_none() {
                return Some(SingleFontFamily::generic(name).1);
            }
        }
        None
    }
}

#[cfg(feature = "gecko")]
/// Iterator of FontFamily
pub struct FontFamilyNameIter<'a> {
    names: &'a structs::nsTArray<structs::FontFamilyName>,
    cur: usize,
}

#[cfg(feature = "gecko")]
impl<'a> Iterator for FontFamilyNameIter<'a> {
    type Item = SingleFontFamily;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.names.len() {
            let item = SingleFontFamily::from_font_family_name(&self.names[self.cur]);
            self.cur += 1;
            Some(item)
        } else {
            None
        }
    }
}

#[derive(Animate, Clone, ComputeSquaredDistance, Copy, Debug, MallocSizeOf, PartialEq, ToCss)]
/// Preserve the readability of text when font fallback occurs
pub enum FontSizeAdjust {
    #[animation(error)]
    /// None variant
    None,
    /// Number variant
    Number(CSSFloat),
}

impl FontSizeAdjust {
    #[inline]
    /// Default value of font-size-adjust
    pub fn none() -> Self {
        FontSizeAdjust::None
    }

    /// Get font-size-adjust with float number
    pub fn from_gecko_adjust(gecko: f32) -> Self {
        if gecko == -1.0 {
            FontSizeAdjust::None
        } else {
            FontSizeAdjust::Number(gecko)
        }
    }
}

impl ToAnimatedZero for FontSizeAdjust {
    #[inline]
    // FIXME(emilio): why?
    fn to_animated_zero(&self) -> Result<Self, ()> {
        Err(())
    }
}

impl ToAnimatedValue for FontSizeAdjust {
    type AnimatedValue = Self;

    #[inline]
    fn to_animated_value(self) -> Self {
        self
    }

    #[inline]
    fn from_animated_value(animated: Self::AnimatedValue) -> Self {
        match animated {
            FontSizeAdjust::Number(number) => FontSizeAdjust::Number(number.max(0.)),
            _ => animated
        }
    }
}

/// Use VariantAlternatesList as computed type of FontVariantAlternates
pub type FontVariantAlternates = specified::VariantAlternatesList;

impl FontVariantAlternates {
    #[inline]
    /// Get initial value with VariantAlternatesList
    pub fn get_initial_value() -> Self {
        specified::VariantAlternatesList(vec![].into_boxed_slice())
    }
}

bitflags! {
    #[derive(MallocSizeOf)]
    /// Vairants for east asian variant
    pub struct FontVariantEastAsian: u16 {
        /// None of the features
        const NORMAL = 0;
        /// Enables rendering of JIS78 forms (OpenType feature: jp78)
        const JIS78 = 0x01;
        /// Enables rendering of JIS83 forms (OpenType feature: jp83).
        const JIS83 = 0x02;
        /// Enables rendering of JIS90 forms (OpenType feature: jp90).
        const JIS90 = 0x04;
        /// Enables rendering of JIS2004 forms (OpenType feature: jp04).
        const JIS04 = 0x08;
        /// Enables rendering of simplified forms (OpenType feature: smpl).
        const SIMPLIFIED = 0x10;
        /// Enables rendering of traditional forms (OpenType feature: trad).
        const TRADITIONAL = 0x20;
        /// Enables rendering of full-width variants (OpenType feature: fwid).
        const FULL_WIDTH = 0x40;
        /// Enables rendering of proportionally-spaced variants (OpenType feature: pwid).
        const PROPORTIONAL_WIDTH = 0x80;
        /// Enables display of ruby variant glyphs (OpenType feature: ruby).
        const RUBY = 0x100;
    }
}

#[cfg(feature = "gecko")]
impl FontVariantEastAsian {
    /// Obtain a specified value from a Gecko keyword value
    ///
    /// Intended for use with presentation attributes, not style structs
    pub fn from_gecko_keyword(kw: u16) -> Self {
        Self::from_bits_truncate(kw)
    }

    /// Transform into gecko keyword
    pub fn to_gecko_keyword(self) -> u16 {
        self.bits()
    }
}

impl ToCss for FontVariantEastAsian {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.is_empty() {
            return dest.write_str("normal")
        }

        let mut has_any = false;

        macro_rules! write_value {
            ($ident:path => $str:expr) => {
                if self.intersects($ident) {
                    if has_any {
                        dest.write_str(" ")?;
                    }
                    has_any = true;
                    dest.write_str($str)?;
                }
            }
        }

        write_value!(FontVariantEastAsian::JIS78 => "jis78");
        write_value!(FontVariantEastAsian::JIS83 => "jis83");
        write_value!(FontVariantEastAsian::JIS90 => "jis90");
        write_value!(FontVariantEastAsian::JIS04 => "jis04");
        write_value!(FontVariantEastAsian::SIMPLIFIED => "simplified");
        write_value!(FontVariantEastAsian::TRADITIONAL => "traditional");
        write_value!(FontVariantEastAsian::FULL_WIDTH => "full-width");
        write_value!(FontVariantEastAsian::PROPORTIONAL_WIDTH => "proportional-width");
        write_value!(FontVariantEastAsian::RUBY => "ruby");

        debug_assert!(has_any);
        Ok(())
    }
}

#[cfg(feature = "gecko")]
impl_gecko_keyword_conversions!(FontVariantEastAsian, u16);

/// Asserts that all variant-east-asian matches its NS_FONT_VARIANT_EAST_ASIAN_* value.
#[cfg(feature = "gecko")]
#[inline]
pub fn assert_variant_east_asian_matches() {
    use gecko_bindings::structs;

    macro_rules! check_variant_east_asian {
        ( $( $a:ident => $b:path),*, ) => {
            if cfg!(debug_assertions) {
                $(
                    assert_eq!(structs::$a as u16, $b.bits());
                )*
            }
        }
    }

    check_variant_east_asian! {
        NS_FONT_VARIANT_EAST_ASIAN_FULL_WIDTH => FontVariantEastAsian::FULL_WIDTH,
        NS_FONT_VARIANT_EAST_ASIAN_JIS04 => FontVariantEastAsian::JIS04,
        NS_FONT_VARIANT_EAST_ASIAN_JIS78 => FontVariantEastAsian::JIS78,
        NS_FONT_VARIANT_EAST_ASIAN_JIS83 => FontVariantEastAsian::JIS83,
        NS_FONT_VARIANT_EAST_ASIAN_JIS90 => FontVariantEastAsian::JIS90,
        NS_FONT_VARIANT_EAST_ASIAN_PROP_WIDTH => FontVariantEastAsian::PROPORTIONAL_WIDTH,
        NS_FONT_VARIANT_EAST_ASIAN_RUBY => FontVariantEastAsian::RUBY,
        NS_FONT_VARIANT_EAST_ASIAN_SIMPLIFIED => FontVariantEastAsian::SIMPLIFIED,
        NS_FONT_VARIANT_EAST_ASIAN_TRADITIONAL => FontVariantEastAsian::TRADITIONAL,
    }
}

bitflags! {
    #[derive(MallocSizeOf)]
    /// Variants of ligatures
    pub struct FontVariantLigatures: u16 {
        /// Specifies that common default features are enabled
        const NORMAL = 0;
        /// Specifies that all types of ligatures and contextual forms
        /// covered by this property are explicitly disabled
        const NONE = 0x01;
        /// Enables display of common ligatures
        const COMMON_LIGATURES = 0x02;
        /// Disables display of common ligatures
        const NO_COMMON_LIGATURES = 0x04;
        /// Enables display of discretionary ligatures
        const DISCRETIONARY_LIGATURES = 0x08;
        /// Disables display of discretionary ligatures
        const NO_DISCRETIONARY_LIGATURES = 0x10;
        /// Enables display of historical ligatures
        const HISTORICAL_LIGATURES = 0x20;
        /// Disables display of historical ligatures
        const NO_HISTORICAL_LIGATURES = 0x40;
        /// Enables display of contextual alternates
        const CONTEXTUAL = 0x80;
        /// Disables display of contextual alternates
        const NO_CONTEXTUAL = 0x100;
    }
}

#[cfg(feature = "gecko")]
impl FontVariantLigatures {
    /// Obtain a specified value from a Gecko keyword value
    ///
    /// Intended for use with presentation attributes, not style structs
    pub fn from_gecko_keyword(kw: u16) -> Self {
        Self::from_bits_truncate(kw)
    }

    /// Transform into gecko keyword
    pub fn to_gecko_keyword(self) -> u16 {
        self.bits()
    }
}

impl ToCss for FontVariantLigatures {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.is_empty() {
            return dest.write_str("normal")
        }
        if self.contains(FontVariantLigatures::NONE) {
            return dest.write_str("none")
        }

        let mut has_any = false;

        macro_rules! write_value {
            ($ident:path => $str:expr) => {
                if self.intersects($ident) {
                    if has_any {
                        dest.write_str(" ")?;
                    }
                    has_any = true;
                    dest.write_str($str)?;
                }
            }
        }

        write_value!(FontVariantLigatures::COMMON_LIGATURES => "common-ligatures");
        write_value!(FontVariantLigatures::NO_COMMON_LIGATURES => "no-common-ligatures");
        write_value!(FontVariantLigatures::DISCRETIONARY_LIGATURES => "discretionary-ligatures");
        write_value!(FontVariantLigatures::NO_DISCRETIONARY_LIGATURES => "no-discretionary-ligatures");
        write_value!(FontVariantLigatures::HISTORICAL_LIGATURES => "historical-ligatures");
        write_value!(FontVariantLigatures::NO_HISTORICAL_LIGATURES => "no-historical-ligatures");
        write_value!(FontVariantLigatures::CONTEXTUAL => "contextual");
        write_value!(FontVariantLigatures::NO_CONTEXTUAL => "no-contextual");

        debug_assert!(has_any);
        Ok(())
    }
}

#[cfg(feature = "gecko")]
impl_gecko_keyword_conversions!(FontVariantLigatures, u16);

/// Asserts that all variant-east-asian matches its NS_FONT_VARIANT_EAST_ASIAN_* value.
#[cfg(feature = "gecko")]
#[inline]
pub fn assert_variant_ligatures_matches() {
    use gecko_bindings::structs;

    macro_rules! check_variant_ligatures {
        ( $( $a:ident => $b:path),*, ) => {
            if cfg!(debug_assertions) {
                $(
                    assert_eq!(structs::$a as u16, $b.bits());
                )*
            }
        }
    }

    check_variant_ligatures! {
        NS_FONT_VARIANT_LIGATURES_NONE => FontVariantLigatures::NONE,
        NS_FONT_VARIANT_LIGATURES_COMMON => FontVariantLigatures::COMMON_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_NO_COMMON => FontVariantLigatures::NO_COMMON_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_DISCRETIONARY => FontVariantLigatures::DISCRETIONARY_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_NO_DISCRETIONARY => FontVariantLigatures::NO_DISCRETIONARY_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_HISTORICAL => FontVariantLigatures::HISTORICAL_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_NO_HISTORICAL => FontVariantLigatures::NO_HISTORICAL_LIGATURES,
        NS_FONT_VARIANT_LIGATURES_CONTEXTUAL => FontVariantLigatures::CONTEXTUAL,
        NS_FONT_VARIANT_LIGATURES_NO_CONTEXTUAL => FontVariantLigatures::NO_CONTEXTUAL,
    }
}

bitflags! {
    #[derive(MallocSizeOf)]
    /// Vairants of numeric values
    pub struct FontVariantNumeric: u8 {
        /// None of other variants are enabled.
        const NORMAL = 0;
        /// Enables display of lining numerals.
        const LINING_NUMS = 0x01;
        /// Enables display of old-style numerals.
        const OLDSTYLE_NUMS = 0x02;
        /// Enables display of proportional numerals.
        const PROPORTIONAL_NUMS = 0x04;
        /// Enables display of tabular numerals.
        const TABULAR_NUMS = 0x08;
        /// Enables display of lining diagonal fractions.
        const DIAGONAL_FRACTIONS = 0x10;
        /// Enables display of lining stacked fractions.
        const STACKED_FRACTIONS = 0x20;
        /// Enables display of letter forms used with ordinal numbers.
        const ORDINAL = 0x80;
        /// Enables display of slashed zeros.
        const SLASHED_ZERO = 0x40;
    }
}

#[cfg(feature = "gecko")]
impl FontVariantNumeric {
    /// Obtain a specified value from a Gecko keyword value
    ///
    /// Intended for use with presentation attributes, not style structs
    pub fn from_gecko_keyword(kw: u8) -> Self {
        Self::from_bits_truncate(kw)
    }

    /// Transform into gecko keyword
    pub fn to_gecko_keyword(self) -> u8 {
        self.bits()
    }
}

impl ToCss for FontVariantNumeric {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.is_empty() {
            return dest.write_str("normal")
        }

        let mut has_any = false;

        macro_rules! write_value {
            ($ident:path => $str:expr) => {
                if self.intersects($ident) {
                    if has_any {
                        dest.write_str(" ")?;
                    }
                    has_any = true;
                    dest.write_str($str)?;
                }
            }
        }

        write_value!(FontVariantNumeric::LINING_NUMS => "lining-nums");
        write_value!(FontVariantNumeric::OLDSTYLE_NUMS => "oldstyle-nums");
        write_value!(FontVariantNumeric::PROPORTIONAL_NUMS => "proportional-nums");
        write_value!(FontVariantNumeric::TABULAR_NUMS => "tabular-nums");
        write_value!(FontVariantNumeric::DIAGONAL_FRACTIONS => "diagonal-fractions");
        write_value!(FontVariantNumeric::STACKED_FRACTIONS => "stacked-fractions");
        write_value!(FontVariantNumeric::SLASHED_ZERO => "slashed-zero");
        write_value!(FontVariantNumeric::ORDINAL => "ordinal");

        debug_assert!(has_any);
        Ok(())
    }
}

#[cfg(feature = "gecko")]
impl_gecko_keyword_conversions!(FontVariantNumeric, u8);

/// Asserts that all variant-east-asian matches its NS_FONT_VARIANT_EAST_ASIAN_* value.
#[cfg(feature = "gecko")]
#[inline]
pub fn assert_variant_numeric_matches() {
    use gecko_bindings::structs;

    macro_rules! check_variant_numeric {
        ( $( $a:ident => $b:path),*, ) => {
            if cfg!(debug_assertions) {
                $(
                    assert_eq!(structs::$a as u8, $b.bits());
                )*
            }
        }
    }

    check_variant_numeric! {
        NS_FONT_VARIANT_NUMERIC_LINING => FontVariantNumeric::LINING_NUMS,
        NS_FONT_VARIANT_NUMERIC_OLDSTYLE => FontVariantNumeric::OLDSTYLE_NUMS,
        NS_FONT_VARIANT_NUMERIC_PROPORTIONAL => FontVariantNumeric::PROPORTIONAL_NUMS,
        NS_FONT_VARIANT_NUMERIC_TABULAR => FontVariantNumeric::TABULAR_NUMS,
        NS_FONT_VARIANT_NUMERIC_DIAGONAL_FRACTIONS => FontVariantNumeric::DIAGONAL_FRACTIONS,
        NS_FONT_VARIANT_NUMERIC_STACKED_FRACTIONS => FontVariantNumeric::STACKED_FRACTIONS,
        NS_FONT_VARIANT_NUMERIC_SLASHZERO => FontVariantNumeric::SLASHED_ZERO,
        NS_FONT_VARIANT_NUMERIC_ORDINAL => FontVariantNumeric::ORDINAL,
    }
}

/// Use FontSettings as computed type of FontFeatureSettings.
pub type FontFeatureSettings = FontSettings<FeatureTagValue<Integer>>;

/// The computed value for font-variation-settings.
pub type FontVariationSettings = FontSettings<VariationValue<Number>>;

/// font-language-override can only have a single three-letter
/// OpenType "language system" tag, so we should be able to compute
/// it and store it as a 32-bit integer
/// (see http://www.microsoft.com/typography/otspec/languagetags.htm).
#[derive(Clone, Copy, Debug, Eq, MallocSizeOf, PartialEq)]
pub struct FontLanguageOverride(pub u32);

impl FontLanguageOverride {
    #[inline]
    /// Get computed default value of `font-language-override` with 0
    pub fn zero() -> FontLanguageOverride {
        FontLanguageOverride(0)
    }
}

impl ToCss for FontLanguageOverride {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result where W: fmt::Write {
        use std::str;

        if self.0 == 0 {
            return dest.write_str("normal")
        }
        let mut buf = [0; 4];
        BigEndian::write_u32(&mut buf, self.0);
        // Safe because we ensure it's ASCII during computing
        let slice = if cfg!(debug_assertions) {
            str::from_utf8(&buf).unwrap()
        } else {
            unsafe { str::from_utf8_unchecked(&buf) }
        };
        slice.trim_right().to_css(dest)
    }
}

#[cfg(feature = "gecko")]
impl From<u32> for FontLanguageOverride {
    fn from(bits: u32) -> FontLanguageOverride {
        FontLanguageOverride(bits)
    }
}

#[cfg(feature = "gecko")]
impl From<FontLanguageOverride> for u32 {
    fn from(v: FontLanguageOverride) -> u32 {
        v.0
    }
}

/// The computed value of the -moz-script-level property.
pub type MozScriptLevel = i8;
