/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Specified values for font properties

use Atom;
use app_units::Au;
use byteorder::{BigEndian, ByteOrder};
use cssparser::{Parser, Token};
#[cfg(feature = "gecko")]
use gecko_bindings::bindings;
#[cfg(feature = "gecko")]
use malloc_size_of::{MallocSizeOf, MallocSizeOfOps};
use parser::{Parse, ParserContext};
use properties::longhands::system_font::SystemFont;
#[allow(unused_imports)]
use std::ascii::AsciiExt;
use std::fmt::{self, Write};
use style_traits::{CssWriter, ParseError, StyleParseErrorKind, ToCss};
use values::CustomIdent;
use values::computed::{Context, Length, NonNegativeLength, ToComputedValue};
use values::computed::font::FontFamily as ComputedFontFamily;
use values::computed::font::FontFamilyList;
use values::computed::font::FontFeatureSettings as ComputedFontFeatureSettings;
use values::computed::font::FontLanguageOverride as ComputedFontLanguageOverride;
use values::computed::font::FontSize as ComputedFontSize;
use values::computed::font::FontSizeAdjust as ComputedFontSizeAdjust;
use values::computed::font::FontVariantAlternates as ComputedFontVariantAlternates;
use values::computed::font::FontVariantEastAsian as ComputedFontVariantEastAsian;
use values::computed::font::FontVariantLigatures as ComputedFontVariantLigatures;
use values::computed::font::FontVariantNumeric as ComputedFontVariantNumeric;
use values::computed::font::FontWeight as ComputedFontWeight;
#[cfg(feature = "gecko")]
use values::computed::font::MozScriptLevel as ComputedMozScriptLevel;
use values::computed::font::MozScriptMinSize as ComputedMozScriptMinSize;
use values::computed::font::SingleFontFamily;
use values::generics::font::{FamilyName, FeatureTagValue, FontSettings, FontTag};
use values::generics::font::{KeywordInfo as GenericKeywordInfo, KeywordSize, VariationValue};
use values::specified::{AllowQuirks, Integer, LengthOrPercentage, NoCalcLength, Number};
use values::specified::length::{AU_PER_PT, AU_PER_PX, FontBaseSize};

const DEFAULT_SCRIPT_MIN_SIZE_PT: u32 = 8;
const DEFAULT_SCRIPT_SIZE_MULTIPLIER: f64 = 0.71;

#[derive(Clone, Copy, Debug, Eq, MallocSizeOf, PartialEq, ToCss)]
/// A specified font-weight value
pub enum FontWeight {
    /// Normal variant
    Normal,
    /// Bold variant
    Bold,
    /// Bolder variant
    Bolder,
    /// Lighter variant
    Lighter,
    /// Computed weight variant
    Weight(ComputedFontWeight),
    /// System font varaint
    System(SystemFont),
}

impl FontWeight {
    /// Get a specified FontWeight from a gecko keyword
    pub fn from_gecko_keyword(kw: u32) -> Self {
        ComputedFontWeight::from_int(kw as i32).map(FontWeight::Weight)
            .expect("Found unexpected value in style struct for font-weight property")
    }

    /// Get a specified FontWeight from a SystemFont
    pub fn system_font(f: SystemFont) -> Self {
        FontWeight::System(f)
    }

    /// Retreive a SystemFont from FontWeight
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontWeight::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl Parse for FontWeight {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontWeight, ParseError<'i>> {
        let result = match *input.next()? {
            Token::Ident(ref ident) => {
                match_ignore_ascii_case! { ident,
                    "normal" => Ok(FontWeight::Normal),
                    "bold" => Ok(FontWeight::Bold),
                    "bolder" => Ok(FontWeight::Bolder),
                    "lighter" => Ok(FontWeight::Lighter),
                    _ => Err(()),
                }
            }
            Token::Number { int_value: Some(value), .. } => {
                ComputedFontWeight::from_int(value).map(FontWeight::Weight)
            },
            _ => Err(()),
        };

        result.map_err(|_| input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
    }
}

impl ToComputedValue for FontWeight {
    type ComputedValue = ComputedFontWeight;

    #[inline]
    fn to_computed_value(&self, context: &Context) -> Self::ComputedValue {
        match *self {
            FontWeight::Weight(weight) => weight,
            FontWeight::Normal => ComputedFontWeight::normal(),
            FontWeight::Bold => ComputedFontWeight::bold(),
            FontWeight::Bolder => {
                context.builder.get_parent_font().clone_font_weight().bolder()
            },
            FontWeight::Lighter => {
                context.builder.get_parent_font().clone_font_weight().lighter()
            },
            #[cfg(feature = "gecko")]
            FontWeight::System(_) => {
                context.cached_system_font.as_ref().unwrap().font_weight.clone()
            },
            #[cfg(not(feature = "gecko"))]
            FontWeight::System(_) => unreachable!(),
        }
    }

    #[inline]
    fn from_computed_value(computed: &ComputedFontWeight) -> Self {
        FontWeight::Weight(*computed)
    }
}

#[derive(Clone, Debug, MallocSizeOf, PartialEq)]
/// A specified font-size value
pub enum FontSize {
    /// A length; e.g. 10px.
    Length(LengthOrPercentage),
    /// A keyword value, along with a ratio and absolute offset.
    /// The ratio in any specified keyword value
    /// will be 1 (with offset 0), but we cascade keywordness even
    /// after font-relative (percent and em) values
    /// have been applied, which is where the ratio
    /// comes in. The offset comes in if we cascaded a calc value,
    /// where the font-relative portion (em and percentage) will
    /// go into the ratio, and the remaining units all computed together
    /// will go into the offset.
    /// See bug 1355707.
    Keyword(KeywordInfo),
    /// font-size: smaller
    Smaller,
    /// font-size: larger
    Larger,
    /// Derived from a specified system font.
    System(SystemFont)
}

impl ToCss for FontSize {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        match *self {
            FontSize::Length(ref lop) => lop.to_css(dest),
            FontSize::Keyword(info) => info.kw.to_css(dest),
            FontSize::Smaller => dest.write_str("smaller"),
            FontSize::Larger => dest.write_str("larger"),
            FontSize::System(sys) => sys.to_css(dest),
        }
    }
}

impl From<LengthOrPercentage> for FontSize {
    fn from(other: LengthOrPercentage) -> Self {
        FontSize::Length(other)
    }
}

/// Specifies a prioritized list of font family names or generic family names.
#[derive(Clone, Debug, Eq, Hash, PartialEq, ToCss)]
pub enum FontFamily {
    /// List of `font-family`
    #[css(iterable, comma)]
    Values(FontFamilyList),
    /// System font
    System(SystemFont),
}

impl FontFamily {
    /// Get `font-family` with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontFamily::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontFamily::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }

    /// Parse a specified font-family value
    pub fn parse_specified<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i>> {
        input.parse_comma_separated(|input| SingleFontFamily::parse(input)).map(|v| {
            FontFamily::Values(FontFamilyList::new(v.into_boxed_slice()))
        })
    }

    #[cfg(feature = "gecko")]
    /// Return the generic ID if it is a single generic font
    pub fn single_generic(&self) -> Option<u8> {
        match *self {
            FontFamily::Values(ref values) => values.single_generic(),
            _ => None,
        }
    }
}

impl ToComputedValue for FontFamily {
    type ComputedValue = ComputedFontFamily;

    fn to_computed_value(&self, _cx: &Context) -> Self::ComputedValue {
        match *self {
            FontFamily::Values(ref v) => ComputedFontFamily(v.clone()),
            FontFamily::System(_) => {
                #[cfg(feature = "gecko")] {
                    _cx.cached_system_font.as_ref().unwrap().font_family.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &ComputedFontFamily) -> Self {
        FontFamily::Values(other.0.clone())
    }
}

#[cfg(feature = "gecko")]
impl MallocSizeOf for FontFamily {
    fn size_of(&self, _ops: &mut MallocSizeOfOps) -> usize {
        match *self {
            FontFamily::Values(ref v) => {
                // Although a SharedFontList object is refcounted, we always
                // attribute its size to the specified value.
                unsafe {
                    bindings::Gecko_SharedFontList_SizeOfIncludingThis(
                        v.0.get()
                    )
                }
            }
            FontFamily::System(_) => 0,
        }
    }
}

impl Parse for FontFamily {
    /// <family-name>#
    /// <family-name> = <string> | [ <ident>+ ]
    /// TODO: <generic-family>
    fn parse<'i, 't>(
        _: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<FontFamily, ParseError<'i>> {
        FontFamily::parse_specified(input)
    }
}

/// `FamilyName::parse` is based on `SingleFontFamily::parse` and not the other way around
/// because we want the former to exclude generic family keywords.
impl Parse for FamilyName {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i>> {
        match SingleFontFamily::parse(input) {
            Ok(SingleFontFamily::FamilyName(name)) => Ok(name),
            Ok(SingleFontFamily::Generic(_)) => Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError)),
            Err(e) => Err(e)
        }
    }
}

#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToCss)]
/// Preserve the readability of text when font fallback occurs
pub enum FontSizeAdjust {
    /// None variant
    None,
    /// Number variant
    Number(Number),
    /// system font
    System(SystemFont),
}

impl FontSizeAdjust {
    #[inline]
    /// Default value of font-size-adjust
    pub fn none() -> Self {
        FontSizeAdjust::None
    }

    /// Get font-size-adjust with SystemFont
    pub fn system_font(f: SystemFont) -> Self {
        FontSizeAdjust::System(f)
    }

    /// Get SystemFont variant
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontSizeAdjust::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontSizeAdjust {
    type ComputedValue = ComputedFontSizeAdjust;

    fn to_computed_value(&self, context: &Context) -> Self::ComputedValue {
        match *self {
            FontSizeAdjust::None => ComputedFontSizeAdjust::None,
            FontSizeAdjust::Number(ref n) => ComputedFontSizeAdjust::Number(n.to_computed_value(context)),
            FontSizeAdjust::System(_) => {
                #[cfg(feature = "gecko")] {
                    context.cached_system_font.as_ref().unwrap().font_size_adjust
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(computed: &Self::ComputedValue) -> Self {
        match *computed {
            ComputedFontSizeAdjust::None => FontSizeAdjust::None,
            ComputedFontSizeAdjust::Number(ref v) => FontSizeAdjust::Number(Number::from_computed_value(v)),
        }
    }
}

impl Parse for FontSizeAdjust {
    /// none | <number>
    fn parse<'i, 't>(context: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontSizeAdjust, ParseError<'i>> {
        if input.try(|input| input.expect_ident_matching("none")).is_ok() {
            return Ok(FontSizeAdjust::None);
        }

        Ok(FontSizeAdjust::Number(Number::parse_non_negative(context, input)?))
    }
}

/// Additional information for specified keyword-derived font sizes.
pub type KeywordInfo = GenericKeywordInfo<NonNegativeLength>;

impl KeywordInfo {
    /// Computes the final size for this font-size keyword, accounting for
    /// text-zoom.
    pub fn to_computed_value(&self, context: &Context) -> NonNegativeLength {
        let base = context.maybe_zoom_text(self.kw.to_computed_value(context));
        base.scale_by(self.factor) + context.maybe_zoom_text(self.offset)
    }

    /// Given a parent keyword info (self), apply an additional factor/offset to it
    pub fn compose(self, factor: f32, offset: NonNegativeLength) -> Self {
        KeywordInfo {
            kw: self.kw,
            factor: self.factor * factor,
            offset: self.offset.scale_by(factor) + offset,
        }
    }
}

impl KeywordSize {
    /// Parses a keyword size.
    pub fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i>> {
        try_match_ident_ignore_ascii_case! { input,
            "xx-small" => Ok(KeywordSize::XXSmall),
            "x-small" => Ok(KeywordSize::XSmall),
            "small" => Ok(KeywordSize::Small),
            "medium" => Ok(KeywordSize::Medium),
            "large" => Ok(KeywordSize::Large),
            "x-large" => Ok(KeywordSize::XLarge),
            "xx-large" => Ok(KeywordSize::XXLarge),
        }
    }
}

/// This is the ratio applied for font-size: larger
/// and smaller by both Firefox and Chrome
const LARGER_FONT_SIZE_RATIO: f32 = 1.2;

/// The default font size.
pub const FONT_MEDIUM_PX: i32 = 16;

#[cfg(feature = "servo")]
impl ToComputedValue for KeywordSize {
    type ComputedValue = NonNegativeLength;
    #[inline]
    fn to_computed_value(&self, _: &Context) -> NonNegativeLength {
        // https://drafts.csswg.org/css-fonts-3/#font-size-prop
        match *self {
            KeywordSize::XXSmall => Au::from_px(FONT_MEDIUM_PX) * 3 / 5,
            KeywordSize::XSmall => Au::from_px(FONT_MEDIUM_PX) * 3 / 4,
            KeywordSize::Small => Au::from_px(FONT_MEDIUM_PX) * 8 / 9,
            KeywordSize::Medium => Au::from_px(FONT_MEDIUM_PX),
            KeywordSize::Large => Au::from_px(FONT_MEDIUM_PX) * 6 / 5,
            KeywordSize::XLarge => Au::from_px(FONT_MEDIUM_PX) * 3 / 2,
            KeywordSize::XXLarge => Au::from_px(FONT_MEDIUM_PX) * 2,
            KeywordSize::XXXLarge => Au::from_px(FONT_MEDIUM_PX) * 3,
        }.into()
    }

    #[inline]
    fn from_computed_value(_: &NonNegativeLength) -> Self {
        unreachable!()
    }
}

#[cfg(feature = "gecko")]
impl ToComputedValue for KeywordSize {
    type ComputedValue = NonNegativeLength;
    #[inline]
    fn to_computed_value(&self, cx: &Context) -> NonNegativeLength {
        use context::QuirksMode;
        use values::specified::length::au_to_int_px;
        // Data from nsRuleNode.cpp in Gecko
        // Mapping from base size and HTML size to pixels
        // The first index is (base_size - 9), the second is the
        // HTML size. "0" is CSS keyword xx-small, not HTML size 0,
        // since HTML size 0 is the same as 1.
        //
        //  xxs   xs      s      m     l      xl     xxl   -
        //  -     0/1     2      3     4      5      6     7
        static FONT_SIZE_MAPPING: [[i32; 8]; 8] = [
            [9,    9,     9,     9,    11,    14,    18,    27],
            [9,    9,     9,    10,    12,    15,    20,    30],
            [9,    9,    10,    11,    13,    17,    22,    33],
            [9,    9,    10,    12,    14,    18,    24,    36],
            [9,   10,    12,    13,    16,    20,    26,    39],
            [9,   10,    12,    14,    17,    21,    28,    42],
            [9,   10,    13,    15,    18,    23,    30,    45],
            [9,   10,    13,    16,    18,    24,    32,    48]
        ];

        // Data from nsRuleNode.cpp in Gecko
        // (https://dxr.mozilla.org/mozilla-central/rev/35fbf14b9/layout/style/nsRuleNode.cpp#3303)
        //
        // This table gives us compatibility with WinNav4 for the default fonts only.
        // In WinNav4, the default fonts were:
        //
        //     Times/12pt ==   Times/16px at 96ppi
        //   Courier/10pt == Courier/13px at 96ppi
        //
        // xxs   xs     s      m      l     xl     xxl    -
        // -     1      2      3      4     5      6      7
        static QUIRKS_FONT_SIZE_MAPPING: [[i32; 8]; 8] = [
            [9,    9,     9,     9,    11,    14,    18,    28],
            [9,    9,     9,    10,    12,    15,    20,    31],
            [9,    9,     9,    11,    13,    17,    22,    34],
            [9,    9,    10,    12,    14,    18,    24,    37],
            [9,    9,    10,    13,    16,    20,    26,    40],
            [9,    9,    11,    14,    17,    21,    28,    42],
            [9,   10,    12,    15,    17,    23,    30,    45],
            [9,   10,    13,    16,    18,    24,    32,    48]
        ];

        static FONT_SIZE_FACTORS: [i32; 8] = [60, 75, 89, 100, 120, 150, 200, 300];

        let ref gecko_font = cx.style().get_font().gecko();
        let base_size = unsafe { Atom::with(gecko_font.mLanguage.mRawPtr, |atom| {
            cx.font_metrics_provider.get_size(atom, gecko_font.mGenericID).0
        }) };

        let base_size_px = au_to_int_px(base_size as f32);
        let html_size = self.html_size() as usize;
        if base_size_px >= 9 && base_size_px <= 16 {
            let mapping = if cx.quirks_mode == QuirksMode::Quirks {
                QUIRKS_FONT_SIZE_MAPPING
            } else {
                FONT_SIZE_MAPPING
            };
            Au::from_px(mapping[(base_size_px - 9) as usize][html_size]).into()
        } else {
            Au(FONT_SIZE_FACTORS[html_size] * base_size / 100).into()
        }
    }

    #[inline]
    fn from_computed_value(_: &NonNegativeLength) -> Self {
        unreachable!()
    }
}

impl FontSize {
    /// <https://html.spec.whatwg.org/multipage/#rules-for-parsing-a-legacy-font-size>
    pub fn from_html_size(size: u8) -> Self {
        FontSize::Keyword(match size {
            // If value is less than 1, let it be 1.
            0 | 1 => KeywordSize::XSmall,
            2 => KeywordSize::Small,
            3 => KeywordSize::Medium,
            4 => KeywordSize::Large,
            5 => KeywordSize::XLarge,
            6 => KeywordSize::XXLarge,
            // If value is greater than 7, let it be 7.
            _ => KeywordSize::XXXLarge,
        }.into())
    }

    /// Compute it against a given base font size
    pub fn to_computed_value_against(
        &self,
        context: &Context,
        base_size: FontBaseSize,
    ) -> ComputedFontSize {
        use values::specified::length::FontRelativeLength;

        let compose_keyword = |factor| {
            context.style().get_parent_font()
                   .clone_font_size().keyword_info
                   .map(|i| i.compose(factor, Au(0).into()))
        };
        let mut info = None;
        let size = match *self {
            FontSize::Length(LengthOrPercentage::Length(
                    NoCalcLength::FontRelative(value))) => {
                if let FontRelativeLength::Em(em) = value {
                    // If the parent font was keyword-derived, this is too.
                    // Tack the em unit onto the factor
                    info = compose_keyword(em);
                }
                value.to_computed_value(context, base_size).into()
            }
            FontSize::Length(LengthOrPercentage::Length(
                    NoCalcLength::ServoCharacterWidth(value))) => {
                value.to_computed_value(base_size.resolve(context)).into()
            }
            FontSize::Length(LengthOrPercentage::Length(
                    NoCalcLength::Absolute(ref l))) => {
                context.maybe_zoom_text(l.to_computed_value(context).into())
            }
            FontSize::Length(LengthOrPercentage::Length(ref l)) => {
                l.to_computed_value(context).into()
            }
            FontSize::Length(LengthOrPercentage::Percentage(pc)) => {
                // If the parent font was keyword-derived, this is too.
                // Tack the % onto the factor
                info = compose_keyword(pc.0);
                base_size.resolve(context).scale_by(pc.0).into()
            }
            FontSize::Length(LengthOrPercentage::Calc(ref calc)) => {
                let parent = context.style().get_parent_font().clone_font_size();
                // if we contain em/% units and the parent was keyword derived, this is too
                // Extract the ratio/offset and compose it
                if (calc.em.is_some() || calc.percentage.is_some()) && parent.keyword_info.is_some() {
                    let ratio = calc.em.unwrap_or(0.) + calc.percentage.map_or(0., |pc| pc.0);
                    // Compute it, but shave off the font-relative part (em, %).
                    //
                    // This will mean that other font-relative units like ex and
                    // ch will be computed against the old parent font even when
                    // the font changes.
                    //
                    // There's no particular "right answer" for what to do here,
                    // Gecko recascades as if the font had changed, we instead
                    // track the changes and reapply, which means that we carry
                    // over old computed ex/ch values whilst Gecko recomputes
                    // new ones.
                    //
                    // This is enough of an edge case to not really matter.
                    let abs = calc.to_computed_value_zoomed(
                        context,
                        FontBaseSize::InheritedStyleButStripEmUnits,
                    ).length_component();

                    info = parent.keyword_info.map(|i| i.compose(ratio, abs.into()));
                }
                let calc = calc.to_computed_value_zoomed(context, base_size);
                calc.to_used_value(Some(base_size.resolve(context))).unwrap().into()
            }
            FontSize::Keyword(i) => {
                // As a specified keyword, this is keyword derived
                info = Some(i);
                i.to_computed_value(context)
            }
            FontSize::Smaller => {
                info = compose_keyword(1. / LARGER_FONT_SIZE_RATIO);
                FontRelativeLength::Em(1. / LARGER_FONT_SIZE_RATIO)
                    .to_computed_value(context, base_size).into()
            }
            FontSize::Larger => {
                info = compose_keyword(LARGER_FONT_SIZE_RATIO);
                FontRelativeLength::Em(LARGER_FONT_SIZE_RATIO)
                    .to_computed_value(context, base_size).into()
            }

            FontSize::System(_) => {
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
                #[cfg(feature = "gecko")] {
                    context.cached_system_font.as_ref().unwrap().font_size.size
                }
            }
        };
        ComputedFontSize {
            size: size,
            keyword_info: info,
        }
    }
}

impl ToComputedValue for FontSize {
    type ComputedValue = ComputedFontSize;

    #[inline]
    fn to_computed_value(&self, context: &Context) -> ComputedFontSize {
        self.to_computed_value_against(context, FontBaseSize::InheritedStyle)
    }

    #[inline]
    fn from_computed_value(computed: &ComputedFontSize) -> Self {
        FontSize::Length(LengthOrPercentage::Length(
            ToComputedValue::from_computed_value(&computed.size.0)
        ))
    }
}

impl FontSize {
    /// Construct a system font value.
    pub fn system_font(f: SystemFont) -> Self {
        FontSize::System(f)
    }

    /// Obtain the system font, if any
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontSize::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }

    #[inline]
    /// Get initial value for specified font size.
    pub fn medium() -> Self {
        FontSize::Keyword(KeywordInfo::medium())
    }

    /// Parses a font-size, with quirks.
    pub fn parse_quirky<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
        allow_quirks: AllowQuirks
    ) -> Result<FontSize, ParseError<'i>> {
        if let Ok(lop) = input.try(|i| LengthOrPercentage::parse_non_negative_quirky(context, i, allow_quirks)) {
            return Ok(FontSize::Length(lop))
        }

        if let Ok(kw) = input.try(KeywordSize::parse) {
            return Ok(FontSize::Keyword(kw.into()))
        }

        try_match_ident_ignore_ascii_case! { input,
            "smaller" => Ok(FontSize::Smaller),
            "larger" => Ok(FontSize::Larger),
        }
    }

    #[allow(unused_mut)]
    /// Cascade `font-size` with specified value
    pub fn cascade_specified_font_size(
        context: &mut Context,
        specified_value: &FontSize,
        mut computed: ComputedFontSize
    ) {
        // we could use clone_language and clone_font_family() here but that's
        // expensive. Do it only in gecko mode for now.
        #[cfg(feature = "gecko")] {
            // if the language or generic changed, we need to recalculate
            // the font size from the stored font-size origin information.
            if context.builder.get_font().gecko().mLanguage.mRawPtr !=
               context.builder.get_parent_font().gecko().mLanguage.mRawPtr ||
               context.builder.get_font().gecko().mGenericID !=
               context.builder.get_parent_font().gecko().mGenericID {
                if let Some(info) = computed.keyword_info {
                    computed.size = info.to_computed_value(context);
                }
            }
        }

        let device = context.builder.device;
        let mut font = context.builder.take_font();
        let parent_unconstrained = {
            let parent_font = context.builder.get_parent_font();
            font.apply_font_size(computed, parent_font, device)
        };
        context.builder.put_font(font);

        if let Some(parent) = parent_unconstrained {
            let new_unconstrained =
                specified_value.to_computed_value_against(context, FontBaseSize::Custom(Au::from(parent)));
            context.builder
                   .mutate_font()
                   .apply_unconstrained_font_size(new_unconstrained.size);
        }
    }
}

impl Parse for FontSize {
    /// <length> | <percentage> | <absolute-size> | <relative-size>
    fn parse<'i, 't>(context: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontSize, ParseError<'i>> {
        FontSize::parse_quirky(context, input, AllowQuirks::No)
    }
}

bitflags! {
    #[cfg_attr(feature = "servo", derive(MallocSizeOf))]
    /// Flags of variant alternates in bit
    struct VariantAlternatesParsingFlags: u8 {
        /// None of variant alternates enabled
        const NORMAL = 0;
        /// Historical forms
        const HISTORICAL_FORMS = 0x01;
        /// Stylistic Alternates
        const STYLISTIC = 0x02;
        /// Stylistic Sets
        const STYLESET = 0x04;
        /// Character Variant
        const CHARACTER_VARIANT = 0x08;
        /// Swash glyphs
        const SWASH = 0x10;
        /// Ornaments glyphs
        const ORNAMENTS = 0x20;
        /// Annotation forms
        const ANNOTATION = 0x40;
    }
}

#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToCss)]
/// Set of variant alternates
pub enum VariantAlternates {
    /// Enables display of stylistic alternates
    #[css(function)]
    Stylistic(CustomIdent),
    /// Enables display with stylistic sets
    #[css(comma, function, iterable)]
    Styleset(Box<[CustomIdent]>),
    /// Enables display of specific character variants
    #[css(comma, function, iterable)]
    CharacterVariant(Box<[CustomIdent]>),
    /// Enables display of swash glyphs
    #[css(function)]
    Swash(CustomIdent),
    /// Enables replacement of default glyphs with ornaments
    #[css(function)]
    Ornaments(CustomIdent),
    /// Enables display of alternate annotation forms
    #[css(function)]
    Annotation(CustomIdent),
    /// Enables display of historical forms
    HistoricalForms,
}

#[derive(Clone, Debug, MallocSizeOf, PartialEq)]
/// List of Variant Alternates
pub struct VariantAlternatesList(pub Box<[VariantAlternates]>);

impl VariantAlternatesList {
    /// Returns the length of all variant alternates.
    pub fn len(&self) -> usize {
        self.0.iter().fold(0, |acc, alternate| {
            match *alternate {
                VariantAlternates::Swash(_) | VariantAlternates::Stylistic(_) |
                VariantAlternates::Ornaments(_) | VariantAlternates::Annotation(_) => {
                    acc + 1
                },
                VariantAlternates::Styleset(ref slice) |
                VariantAlternates::CharacterVariant(ref slice) => {
                    acc + slice.len()
                },
                _ => acc,
            }
        })
    }
}

impl ToCss for VariantAlternatesList {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.0.is_empty() {
            return dest.write_str("normal");
        }

        let mut iter = self.0.iter();
        iter.next().unwrap().to_css(dest)?;
        for alternate in iter {
            dest.write_str(" ")?;
            alternate.to_css(dest)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToCss)]
/// Control over the selection of these alternate glyphs
pub enum FontVariantAlternates {
    /// Use alternative glyph from value
    Value(VariantAlternatesList),
    /// Use system font glyph
    System(SystemFont)
}

impl FontVariantAlternates {
    #[inline]
    /// Get initial specified value with VariantAlternatesList
    pub fn get_initial_specified_value() -> Self {
        FontVariantAlternates::Value(VariantAlternatesList(vec![].into_boxed_slice()))
    }

    /// Get FontVariantAlternates with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontVariantAlternates::System(f)
    }

    /// Get SystemFont of FontVariantAlternates
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontVariantAlternates::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontVariantAlternates {
    type ComputedValue = ComputedFontVariantAlternates;

    fn to_computed_value(&self, _context: &Context) -> Self::ComputedValue {
        match *self {
            FontVariantAlternates::Value(ref v) => v.clone(),
            FontVariantAlternates::System(_) => {
                #[cfg(feature = "gecko")] {
                    _context.cached_system_font.as_ref().unwrap().font_variant_alternates.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        FontVariantAlternates::Value(other.clone())
    }
}

impl Parse for FontVariantAlternates {
    /// normal |
    ///  [ stylistic(<feature-value-name>)           ||
    ///    historical-forms                          ||
    ///    styleset(<feature-value-name> #)          ||
    ///    character-variant(<feature-value-name> #) ||
    ///    swash(<feature-value-name>)               ||
    ///    ornaments(<feature-value-name>)           ||
    ///    annotation(<feature-value-name>) ]
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontVariantAlternates, ParseError<'i>> {
        let mut alternates = Vec::new();
        if input.try(|input| input.expect_ident_matching("normal")).is_ok() {
            return Ok(FontVariantAlternates::Value(VariantAlternatesList(alternates.into_boxed_slice())));
        }

        let mut parsed_alternates = VariantAlternatesParsingFlags::empty();
        macro_rules! check_if_parsed(
            ($input:expr, $flag:path) => (
                if parsed_alternates.contains($flag) {
                    return Err($input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
                }
                parsed_alternates |= $flag;
            )
        );
        while let Ok(_) = input.try(|input| {
            // FIXME: remove clone() when lifetimes are non-lexical
            match input.next()?.clone() {
                Token::Ident(ref value) if value.eq_ignore_ascii_case("historical-forms") => {
                    check_if_parsed!(input, VariantAlternatesParsingFlags::HISTORICAL_FORMS);
                    alternates.push(VariantAlternates::HistoricalForms);
                    Ok(())
                },
                Token::Function(ref name) => {
                    input.parse_nested_block(|i| {
                        match_ignore_ascii_case! { &name,
                            "swash" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::SWASH);
                                let location = i.current_source_location();
                                let ident = CustomIdent::from_ident(location, i.expect_ident()?, &[])?;
                                alternates.push(VariantAlternates::Swash(ident));
                                Ok(())
                            },
                            "stylistic" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::STYLISTIC);
                                let location = i.current_source_location();
                                let ident = CustomIdent::from_ident(location, i.expect_ident()?, &[])?;
                                alternates.push(VariantAlternates::Stylistic(ident));
                                Ok(())
                            },
                            "ornaments" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::ORNAMENTS);
                                let location = i.current_source_location();
                                let ident = CustomIdent::from_ident(location, i.expect_ident()?, &[])?;
                                alternates.push(VariantAlternates::Ornaments(ident));
                                Ok(())
                            },
                            "annotation" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::ANNOTATION);
                                let location = i.current_source_location();
                                let ident = CustomIdent::from_ident(location, i.expect_ident()?, &[])?;
                                alternates.push(VariantAlternates::Annotation(ident));
                                Ok(())
                            },
                            "styleset" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::STYLESET);
                                let idents = i.parse_comma_separated(|i| {
                                    let location = i.current_source_location();
                                    CustomIdent::from_ident(location, i.expect_ident()?, &[])
                                })?;
                                alternates.push(VariantAlternates::Styleset(idents.into_boxed_slice()));
                                Ok(())
                            },
                            "character-variant" => {
                                check_if_parsed!(i, VariantAlternatesParsingFlags::CHARACTER_VARIANT);
                                let idents = i.parse_comma_separated(|i| {
                                    let location = i.current_source_location();
                                    CustomIdent::from_ident(location, i.expect_ident()?, &[])
                                })?;
                                alternates.push(VariantAlternates::CharacterVariant(idents.into_boxed_slice()));
                                Ok(())
                            },
                            _ => return Err(i.new_custom_error(StyleParseErrorKind::UnspecifiedError)),
                        }
                    })
                },
                _ => Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError)),
            }
        }) { }

        if parsed_alternates.is_empty() {
            return Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError));
        }
        Ok(FontVariantAlternates::Value(VariantAlternatesList(alternates.into_boxed_slice())))
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Debug, PartialEq, ToCss)]
/// Allows control of glyph substitution and sizing in East Asian text.
pub enum FontVariantEastAsian {
    /// Value variant with `variant-east-asian`
    Value(ComputedFontVariantEastAsian),
    /// System font variant
    System(SystemFont)
}

impl FontVariantEastAsian {
    #[inline]
    /// Get default `font-variant-east-asian` with `empty` variant
    pub fn empty() -> Self {
        FontVariantEastAsian::Value(ComputedFontVariantEastAsian::empty())
    }

    /// Get `font-variant-east-asian` with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontVariantEastAsian::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontVariantEastAsian::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontVariantEastAsian {
    type ComputedValue = ComputedFontVariantEastAsian;

    fn to_computed_value(&self, _context: &Context) -> Self::ComputedValue {
        match *self {
            FontVariantEastAsian::Value(ref v) => v.clone(),
            FontVariantEastAsian::System(_) => {
                #[cfg(feature = "gecko")] {
                    _context.cached_system_font.as_ref().unwrap().font_variant_east_asian.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        FontVariantEastAsian::Value(other.clone())
    }
}

impl Parse for FontVariantEastAsian {
    /// normal | [ <east-asian-variant-values> || <east-asian-width-values> || ruby ]
    /// <east-asian-variant-values> = [ jis78 | jis83 | jis90 | jis04 | simplified | traditional ]
    /// <east-asian-width-values>   = [ full-width | proportional-width ]
    fn parse<'i, 't>(
        _context: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<FontVariantEastAsian, ParseError<'i>> {
        let mut result = ComputedFontVariantEastAsian::empty();

        if input.try(|input| input.expect_ident_matching("normal")).is_ok() {
            return Ok(FontVariantEastAsian::Value(result))
        }

        while let Ok(flag) = input.try(|input| {
            Ok(match_ignore_ascii_case! { &input.expect_ident().map_err(|_| ())?,
                "jis78" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::JIS78
                        }
                    )
                }
                "jis83" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::JIS83
                        }
                    )
                }
                "jis90" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::JIS90
                        }
                    )
                }
                "jis04" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::JIS04
                        }
                    )
                }
                "simplified" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::SIMPLIFIED
                        }
                    )
                }
                "traditional" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::JIS78 |
                            ComputedFontVariantEastAsian::JIS83 |
                            ComputedFontVariantEastAsian::JIS90 |
                            ComputedFontVariantEastAsian::JIS04 |
                            ComputedFontVariantEastAsian::SIMPLIFIED |
                            ComputedFontVariantEastAsian::TRADITIONAL
                        ) => {
                            ComputedFontVariantEastAsian::TRADITIONAL
                        }
                    )
                }
                "full-width" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::FULL_WIDTH |
                            ComputedFontVariantEastAsian::PROPORTIONAL_WIDTH
                        ) => {
                            ComputedFontVariantEastAsian::FULL_WIDTH
                        }
                    )
                }
                "proportional-width" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantEastAsian::FULL_WIDTH |
                            ComputedFontVariantEastAsian::PROPORTIONAL_WIDTH
                        ) => {
                            ComputedFontVariantEastAsian::PROPORTIONAL_WIDTH
                        }
                    )
                }
                "ruby" => {
                    exclusive_value!(
                        (result, ComputedFontVariantEastAsian::RUBY) => {
                            ComputedFontVariantEastAsian::RUBY
                        }
                    )
                }
                _ => return Err(()),
            })
        }) {
            result.insert(flag);
        }

        if !result.is_empty() {
            Ok(FontVariantEastAsian::Value(result))
        } else {
            Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
        }
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Debug, PartialEq, ToCss)]
/// Ligatures and contextual forms are ways of combining glyphs
/// to produce more harmonized forms
pub enum FontVariantLigatures {
    /// Value variant with `variant-ligatures`
    Value(ComputedFontVariantLigatures),
    /// System font variant
    System(SystemFont)
}

impl FontVariantLigatures {
    /// Get `font-variant-ligatures` with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontVariantLigatures::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontVariantLigatures::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }

    #[inline]
    /// Default value of `font-variant-ligatures` as `empty`
    pub fn empty() -> FontVariantLigatures {
        FontVariantLigatures::Value(ComputedFontVariantLigatures::empty())
    }

    #[inline]
    /// Get `none` variant of `font-variant-ligatures`
    pub fn none() -> FontVariantLigatures {
        FontVariantLigatures::Value(ComputedFontVariantLigatures::NONE)
    }
}

impl ToComputedValue for FontVariantLigatures {
    type ComputedValue = ComputedFontVariantLigatures;

    fn to_computed_value(&self, _context: &Context) -> Self::ComputedValue {
        match *self {
            FontVariantLigatures::Value(ref v) => v.clone(),
            FontVariantLigatures::System(_) => {
                #[cfg(feature = "gecko")] {
                    _context.cached_system_font.as_ref().unwrap().font_variant_ligatures.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        FontVariantLigatures::Value(other.clone())
    }
}

impl Parse for FontVariantLigatures {
    /// normal | none |
    /// [ <common-lig-values> ||
    ///   <discretionary-lig-values> ||
    ///   <historical-lig-values> ||
    ///   <contextual-alt-values> ]
    /// <common-lig-values>        = [ common-ligatures | no-common-ligatures ]
    /// <discretionary-lig-values> = [ discretionary-ligatures | no-discretionary-ligatures ]
    /// <historical-lig-values>    = [ historical-ligatures | no-historical-ligatures ]
    /// <contextual-alt-values>    = [ contextual | no-contextual ]
    fn parse<'i, 't> (
        _context: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<FontVariantLigatures, ParseError<'i>> {
        let mut result = ComputedFontVariantLigatures::empty();

        if input.try(|input| input.expect_ident_matching("normal")).is_ok() {
            return Ok(FontVariantLigatures::Value(result))
        }
        if input.try(|input| input.expect_ident_matching("none")).is_ok() {
            return Ok(FontVariantLigatures::Value(ComputedFontVariantLigatures::NONE))
        }

        while let Ok(flag) = input.try(|input| {
            Ok(match_ignore_ascii_case! { &input.expect_ident().map_err(|_| ())?,
                "common-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::COMMON_LIGATURES |
                            ComputedFontVariantLigatures::NO_COMMON_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::COMMON_LIGATURES
                        }
                    )
                }
                "no-common-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::COMMON_LIGATURES |
                            ComputedFontVariantLigatures::NO_COMMON_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::NO_COMMON_LIGATURES
                        }
                    )
                }
                "discretionary-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::DISCRETIONARY_LIGATURES |
                            ComputedFontVariantLigatures::NO_DISCRETIONARY_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::DISCRETIONARY_LIGATURES
                        }
                    )
                }
                "no-discretionary-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::DISCRETIONARY_LIGATURES |
                            ComputedFontVariantLigatures::NO_DISCRETIONARY_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::NO_DISCRETIONARY_LIGATURES
                        }
                    )
                }
                "historical-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::HISTORICAL_LIGATURES |
                            ComputedFontVariantLigatures::NO_HISTORICAL_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::HISTORICAL_LIGATURES
                        }
                    )
                }
                "no-historical-ligatures" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::HISTORICAL_LIGATURES |
                            ComputedFontVariantLigatures::NO_HISTORICAL_LIGATURES
                        ) => {
                            ComputedFontVariantLigatures::NO_HISTORICAL_LIGATURES
                        }
                    )
                }
                "contextual" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::CONTEXTUAL |
                            ComputedFontVariantLigatures::NO_CONTEXTUAL
                        ) => {
                            ComputedFontVariantLigatures::CONTEXTUAL
                        }
                    )
                }
                "no-contextual" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantLigatures::CONTEXTUAL |
                            ComputedFontVariantLigatures::NO_CONTEXTUAL
                        ) => {
                            ComputedFontVariantLigatures::NO_CONTEXTUAL
                        }
                    )
                }
                _ => return Err(()),
            })
        }) {
            result.insert(flag);
        }

        if !result.is_empty() {
            Ok(FontVariantLigatures::Value(result))
        } else {
            Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
        }
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Debug, PartialEq, ToCss)]
/// Specifies control over numerical forms.
pub enum FontVariantNumeric {
    /// Value variant with `variant-numeric`
    Value(ComputedFontVariantNumeric),
    /// System font
    System(SystemFont)
}

impl FontVariantNumeric {
    #[inline]
    /// Default value of `font-variant-numeric` as `empty`
    pub fn empty() -> FontVariantNumeric {
        FontVariantNumeric::Value(ComputedFontVariantNumeric::empty())
    }

    /// Get `font-variant-numeric` with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontVariantNumeric::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontVariantNumeric::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontVariantNumeric {
    type ComputedValue = ComputedFontVariantNumeric;

    fn to_computed_value(&self, _context: &Context) -> Self::ComputedValue {
        match *self {
            FontVariantNumeric::Value(ref v) => v.clone(),
            FontVariantNumeric::System(_) => {
                #[cfg(feature = "gecko")] {
                    _context.cached_system_font.as_ref().unwrap().font_variant_numeric.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        FontVariantNumeric::Value(other.clone())
    }
}

impl Parse for FontVariantNumeric {
    /// normal |
    ///  [ <numeric-figure-values>   ||
    ///    <numeric-spacing-values>  ||
    ///    <numeric-fraction-values> ||
    ///    ordinal                   ||
    ///    slashed-zero ]
    /// <numeric-figure-values>   = [ lining-nums | oldstyle-nums ]
    /// <numeric-spacing-values>  = [ proportional-nums | tabular-nums ]
    /// <numeric-fraction-values> = [ diagonal-fractions | stacked-fractions ]
    fn parse<'i, 't>(
        _context: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<FontVariantNumeric, ParseError<'i>> {
        let mut result = ComputedFontVariantNumeric::empty();

        if input.try(|input| input.expect_ident_matching("normal")).is_ok() {
            return Ok(FontVariantNumeric::Value(result))
        }

        while let Ok(flag) = input.try(|input| {
            Ok(match_ignore_ascii_case! { &input.expect_ident().map_err(|_| ())?,
                "ordinal" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::ORDINAL
                        ) => {
                            ComputedFontVariantNumeric::ORDINAL
                        }
                    )
                }
                "slashed-zero" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::SLASHED_ZERO
                        ) => {
                            ComputedFontVariantNumeric::SLASHED_ZERO
                        }
                    )
                }
                "lining-nums" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::LINING_NUMS |
                            ComputedFontVariantNumeric::OLDSTYLE_NUMS
                        ) => {
                            ComputedFontVariantNumeric::LINING_NUMS
                        }
                    )
                }
                "oldstyle-nums" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::LINING_NUMS |
                            ComputedFontVariantNumeric::OLDSTYLE_NUMS
                        ) => {
                            ComputedFontVariantNumeric::OLDSTYLE_NUMS
                        }
                    )
                }
                "proportional-nums" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::PROPORTIONAL_NUMS |
                            ComputedFontVariantNumeric::TABULAR_NUMS
                        ) => {
                            ComputedFontVariantNumeric::PROPORTIONAL_NUMS
                        }
                    )
                },
                "tabular-nums" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::PROPORTIONAL_NUMS |
                            ComputedFontVariantNumeric::TABULAR_NUMS
                        ) => {
                            ComputedFontVariantNumeric::TABULAR_NUMS
                        }
                    )
                }
                "diagonal-fractions" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::DIAGONAL_FRACTIONS |
                            ComputedFontVariantNumeric::STACKED_FRACTIONS
                        ) => {
                            ComputedFontVariantNumeric::DIAGONAL_FRACTIONS
                        }
                    )
                }
                "stacked-fractions" => {
                    exclusive_value!(
                        (
                            result,
                            ComputedFontVariantNumeric::DIAGONAL_FRACTIONS |
                            ComputedFontVariantNumeric::STACKED_FRACTIONS
                        ) => {
                            ComputedFontVariantNumeric::STACKED_FRACTIONS
                        }
                    )
                }
                _ => return Err(()),
            })
        }) {
            result.insert(flag);
        }

        if !result.is_empty() {
            Ok(FontVariantNumeric::Value(result))
        } else {
            Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
        }
    }
}

/// This property provides low-level control over OpenType or TrueType font variations.
pub type SpecifiedFontFeatureSettings = FontSettings<FeatureTagValue<Integer>>;

/// Define initial settings that apply when the font defined by an @font-face
/// rule is rendered.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToCss)]
pub enum FontFeatureSettings {
    /// Value of `FontSettings`
    Value(SpecifiedFontFeatureSettings),
    /// System font
    System(SystemFont)
}

impl FontFeatureSettings {
    #[inline]
    /// Get default value of `font-feature-settings` as normal
    pub fn normal() -> FontFeatureSettings {
        FontFeatureSettings::Value(FontSettings::normal())
    }

    /// Get `font-feature-settings` with system font
    pub fn system_font(f: SystemFont) -> Self {
        FontFeatureSettings::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontFeatureSettings::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontFeatureSettings {
    type ComputedValue = ComputedFontFeatureSettings;

    fn to_computed_value(&self, context: &Context) -> Self::ComputedValue {
        match *self {
            FontFeatureSettings::Value(ref v) => v.to_computed_value(context),
            FontFeatureSettings::System(_) => {
                #[cfg(feature = "gecko")] {
                    context.cached_system_font.as_ref().unwrap().font_feature_settings.clone()
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        FontFeatureSettings::Value(ToComputedValue::from_computed_value(other))
    }
}

impl Parse for FontFeatureSettings {
    /// normal | <feature-tag-value>#
    fn parse<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<FontFeatureSettings, ParseError<'i>> {
        SpecifiedFontFeatureSettings::parse(context, input).map(FontFeatureSettings::Value)
    }
}

#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToComputedValue)]
/// Whether user agents are allowed to synthesize bold or oblique font faces
/// when a font family lacks bold or italic faces
pub struct FontSynthesis {
    /// If a `font-weight` is requested that the font family does not contain,
    /// the user agent may synthesize the requested weight from the weights
    /// that do exist in the font family.
    pub weight: bool,
    /// If a font-style is requested that the font family does not contain,
    /// the user agent may synthesize the requested style from the normal face in the font family.
    pub style: bool,
}

impl FontSynthesis {
    #[inline]
    /// Get the default value of font-synthesis
    pub fn get_initial_value() -> Self {
        FontSynthesis {
            weight: true,
            style: true
        }
    }
}

impl Parse for FontSynthesis {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontSynthesis, ParseError<'i>> {
        let mut result = FontSynthesis { weight: false, style: false };
        try_match_ident_ignore_ascii_case! { input,
            "none" => Ok(result),
            "weight" => {
                result.weight = true;
                if input.try(|input| input.expect_ident_matching("style")).is_ok() {
                    result.style = true;
                }
                Ok(result)
            },
            "style" => {
                result.style = true;
                if input.try(|input| input.expect_ident_matching("weight")).is_ok() {
                    result.weight = true;
                }
                Ok(result)
            },
        }
    }
}

impl ToCss for FontSynthesis {
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        if self.weight && self.style {
            dest.write_str("weight style")
        } else if self.style {
            dest.write_str("style")
        } else if self.weight {
            dest.write_str("weight")
        } else {
            dest.write_str("none")
        }
    }
}

#[cfg(feature = "gecko")]
impl From<u8> for FontSynthesis {
    fn from(bits: u8) -> FontSynthesis {
        use gecko_bindings::structs;

        FontSynthesis {
            weight: bits & structs::NS_FONT_SYNTHESIS_WEIGHT as u8 != 0,
            style: bits & structs::NS_FONT_SYNTHESIS_STYLE as u8 != 0
        }
    }
}

#[cfg(feature = "gecko")]
impl From<FontSynthesis> for u8 {
    fn from(v: FontSynthesis) -> u8 {
        use gecko_bindings::structs;

        let mut bits: u8 = 0;
        if v.weight {
            bits |= structs::NS_FONT_SYNTHESIS_WEIGHT as u8;
        }
        if v.style {
            bits |= structs::NS_FONT_SYNTHESIS_STYLE as u8;
        }
        bits
    }
}

#[derive(Clone, Debug, Eq, MallocSizeOf, PartialEq, ToCss)]
/// Allows authors to explicitly specify the language system of the font,
/// overriding the language system implied by the content language
pub enum FontLanguageOverride {
    /// When rendering with OpenType fonts,
    /// the content language of the element is
    /// used to infer the OpenType language system
    Normal,
    /// Single three-letter case-sensitive OpenType language system tag,
    /// specifies the OpenType language system to be used instead of
    /// the language system implied by the language of the element
    Override(Box<str>),
    /// Use system font
    System(SystemFont)
}

impl FontLanguageOverride {
    #[inline]
    /// Get default value with `normal`
    pub fn normal() -> FontLanguageOverride {
        FontLanguageOverride::Normal
    }

    /// Get `font-language-override` with `system font`
    pub fn system_font(f: SystemFont) -> Self {
        FontLanguageOverride::System(f)
    }

    /// Get system font
    pub fn get_system(&self) -> Option<SystemFont> {
        if let FontLanguageOverride::System(s) = *self {
            Some(s)
        } else {
            None
        }
    }
}

impl ToComputedValue for FontLanguageOverride {
    type ComputedValue = ComputedFontLanguageOverride;

    #[inline]
    fn to_computed_value(&self, _context: &Context) -> Self::ComputedValue {
        #[allow(unused_imports)] use std::ascii::AsciiExt;
        match *self {
            FontLanguageOverride::Normal => ComputedFontLanguageOverride(0),
            FontLanguageOverride::Override(ref lang) => {
                if lang.is_empty() || lang.len() > 4 || !lang.is_ascii() {
                    return ComputedFontLanguageOverride(0)
                }
                let mut computed_lang = lang.to_string();
                while computed_lang.len() < 4 {
                    computed_lang.push(' ');
                }
                let bytes = computed_lang.into_bytes();
                ComputedFontLanguageOverride(BigEndian::read_u32(&bytes))
            }
            FontLanguageOverride::System(_) => {
                #[cfg(feature = "gecko")] {
                    _context.cached_system_font.as_ref().unwrap().font_language_override
                }
                #[cfg(feature = "servo")] {
                    unreachable!()
                }
            }
        }
    }
    #[inline]
    fn from_computed_value(computed: &Self::ComputedValue) -> Self {
        if computed.0 == 0 {
            return FontLanguageOverride::Normal
        }
        let mut buf = [0; 4];
        BigEndian::write_u32(&mut buf, computed.0);
        FontLanguageOverride::Override(
            if cfg!(debug_assertions) {
                String::from_utf8(buf.to_vec()).unwrap()
            } else {
                unsafe { String::from_utf8_unchecked(buf.to_vec()) }
            }.into_boxed_str()
        )
    }
}

impl Parse for FontLanguageOverride {
    /// normal | <string>
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<FontLanguageOverride, ParseError<'i>> {
        if input.try(|input| input.expect_ident_matching("normal")).is_ok() {
            return Ok(FontLanguageOverride::Normal)
        }

        let string = input.expect_string()?;
        Ok(FontLanguageOverride::Override(string.as_ref().to_owned().into_boxed_str()))
    }
}

impl Parse for FontTag {
    fn parse<'i, 't>(
        _context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i>> {
        use byteorder::{ReadBytesExt, BigEndian};
        use std::io::Cursor;

        let location = input.current_source_location();
        let tag = input.expect_string()?;

        // allowed strings of length 4 containing chars: <U+20, U+7E>
        if tag.len() != 4 || tag.as_bytes().iter().any(|c| *c < b' ' || *c > b'~') {
            return Err(location.new_custom_error(StyleParseErrorKind::UnspecifiedError))
        }

        let mut raw = Cursor::new(tag.as_bytes());
        Ok(FontTag(raw.read_u32::<BigEndian>().unwrap()))
    }
}

/// This property provides low-level control over OpenType or TrueType font
/// variations.
pub type FontVariationSettings = FontSettings<VariationValue<Number>>;

fn parse_one_feature_value<'i, 't>(
    context: &ParserContext,
    input: &mut Parser<'i, 't>,
) -> Result<Integer, ParseError<'i>> {
    if let Ok(integer) = input.try(|i| Integer::parse_non_negative(context, i)) {
        return Ok(integer)
    }

    try_match_ident_ignore_ascii_case! { input,
        "on" => Ok(Integer::new(1)),
        "off" => Ok(Integer::new(0)),
    }
}

impl Parse for FeatureTagValue<Integer> {
    /// https://drafts.csswg.org/css-fonts-4/#feature-tag-value
    fn parse<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i>> {
        let tag = FontTag::parse(context, input)?;
        let value = input.try(|i| parse_one_feature_value(context, i))
            .unwrap_or_else(|_| Integer::new(1));

        Ok(Self { tag, value })
    }
}

impl Parse for VariationValue<Number> {
    /// This is the `<string> <number>` part of the font-variation-settings
    /// syntax.
    fn parse<'i, 't>(
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i>> {
        let tag = FontTag::parse(context, input)?;
        let value = Number::parse(context, input)?;
        Ok(Self { tag, value })
    }
}


#[derive(Clone, Copy, Debug, MallocSizeOf, PartialEq, ToComputedValue)]
/// text-zoom. Enable if true, disable if false
pub struct XTextZoom(pub bool);

impl Parse for XTextZoom {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<XTextZoom, ParseError<'i>> {
        debug_assert!(false, "Should be set directly by presentation attributes only.");
        Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
    }
}

impl ToCss for XTextZoom {
    fn to_css<W>(&self, _: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        Ok(())
    }
}

#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToComputedValue)]
/// Internal property that reflects the lang attribute
pub struct XLang(pub Atom);

impl XLang {
    #[inline]
    /// Get default value for `-x-lang`
    pub fn get_initial_value() -> XLang {
        XLang(atom!(""))
    }
}

impl Parse for XLang {
    fn parse<'i, 't>(
        _: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<XLang, ParseError<'i>> {
        debug_assert!(false, "Should be set directly by presentation attributes only.");
        Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
    }
}

impl ToCss for XLang {
    fn to_css<W>(&self, _: &mut CssWriter<W>) -> fmt::Result
    where
        W: Write,
    {
        Ok(())
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Copy, Debug, PartialEq, ToCss)]
/// Specifies the minimum font size allowed due to changes in scriptlevel.
/// Ref: https://wiki.mozilla.org/MathML:mstyle
pub struct MozScriptMinSize(pub NoCalcLength);

impl MozScriptMinSize {
    #[inline]
    /// Calculate initial value of -moz-script-min-size.
    pub fn get_initial_value() -> Length {
        Length::new(DEFAULT_SCRIPT_MIN_SIZE_PT as f32 * (AU_PER_PT / AU_PER_PX))
    }
}

impl Parse for MozScriptMinSize {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<MozScriptMinSize, ParseError<'i>> {
        debug_assert!(false, "Should be set directly by presentation attributes only.");
        Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
    }
}

impl ToComputedValue for MozScriptMinSize {
    type ComputedValue = ComputedMozScriptMinSize;

    fn to_computed_value(&self, cx: &Context) -> Self::ComputedValue {
        // this value is used in the computation of font-size, so
        // we use the parent size
        let base_size = FontBaseSize::InheritedStyle;
        match self.0 {
            NoCalcLength::FontRelative(value) => {
                value.to_computed_value(cx, base_size)
            }
            NoCalcLength::ServoCharacterWidth(value) => {
                value.to_computed_value(base_size.resolve(cx))
            }
            ref l => {
                l.to_computed_value(cx)
            }
        }
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        MozScriptMinSize(ToComputedValue::from_computed_value(other))
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Copy, Debug, PartialEq, ToCss)]
/// Changes the scriptlevel in effect for the children.
/// Ref: https://wiki.mozilla.org/MathML:mstyle
///
/// The main effect of scriptlevel is to control the font size.
/// https://www.w3.org/TR/MathML3/chapter3.html#presm.scriptlevel
pub enum MozScriptLevel {
    /// Change `font-size` relatively.
    Relative(i32),
    /// Change `font-size` absolutely.
    ///
    /// Should only be serialized by presentation attributes, so even though
    /// serialization for this would look the same as for the `Relative`
    /// variant, it is unexposed, so no big deal.
    #[css(function)]
    MozAbsolute(i32),
    /// Change `font-size` automatically.
    Auto
}

impl Parse for MozScriptLevel {
    fn parse<'i, 't>(_: &ParserContext, input: &mut Parser<'i, 't>) -> Result<MozScriptLevel, ParseError<'i>> {
        // We don't bother to handle calc here.
        if let Ok(i) = input.try(|i| i.expect_integer()) {
            return Ok(MozScriptLevel::Relative(i))
        }
        input.expect_ident_matching("auto")?;
        Ok(MozScriptLevel::Auto)
    }
}

#[cfg(feature = "gecko")]
impl ToComputedValue for MozScriptLevel {
    type ComputedValue = ComputedMozScriptLevel;

    fn to_computed_value(&self, cx: &Context) -> Self::ComputedValue {
        use properties::longhands::_moz_math_display::SpecifiedValue as DisplayValue;
        use std::{cmp, i8};

        let int = match *self {
            MozScriptLevel::Auto => {
                let parent = cx.builder.get_parent_font().clone__moz_script_level() as i32;
                let display = cx.builder.get_parent_font().clone__moz_math_display();
                if display == DisplayValue::Inline {
                    parent + 1
                } else {
                    parent
                }
            }
            MozScriptLevel::Relative(rel) => {
                let parent = cx.builder.get_parent_font().clone__moz_script_level();
                parent as i32 + rel
            }
            MozScriptLevel::MozAbsolute(abs) => abs,
        };
        cmp::min(int, i8::MAX as i32) as Self::ComputedValue
    }

    fn from_computed_value(other: &Self::ComputedValue) -> Self {
        MozScriptLevel::MozAbsolute(*other as i32)
    }
}

#[cfg_attr(feature = "gecko", derive(MallocSizeOf))]
#[derive(Clone, Copy, Debug, PartialEq, ToComputedValue, ToCss)]
/// Specifies the multiplier to be used to adjust font size
/// due to changes in scriptlevel.
///
/// Ref: https://www.w3.org/TR/MathML3/chapter3.html#presm.mstyle.attrs
pub struct MozScriptSizeMultiplier(pub f32);

impl MozScriptSizeMultiplier {
    #[inline]
    /// Get default value of `-moz-script-size-multiplier`
    pub fn get_initial_value() -> MozScriptSizeMultiplier {
        MozScriptSizeMultiplier(DEFAULT_SCRIPT_SIZE_MULTIPLIER as f32)
    }
}

impl Parse for MozScriptSizeMultiplier {
    fn parse<'i, 't>(
        _: &ParserContext,
        input: &mut Parser<'i, 't>
    ) -> Result<MozScriptSizeMultiplier, ParseError<'i>> {
        debug_assert!(false, "Should be set directly by presentation attributes only.");
        Err(input.new_custom_error(StyleParseErrorKind::UnspecifiedError))
    }
}

impl From<f32> for MozScriptSizeMultiplier {
    fn from(v: f32) -> Self {
        MozScriptSizeMultiplier(v)
    }
}

impl From<MozScriptSizeMultiplier> for f32 {
    fn from(v: MozScriptSizeMultiplier) -> f32 {
        v.0
    }
}
