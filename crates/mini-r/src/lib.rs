pub mod rfunc_intrinsic;

use chumsky::prelude::*;
use miette::miette;
use rfunc_intrinsic::{RFuncSignature, RFunctionDef};

use std::{borrow::Cow, collections::HashMap};
use text::inline_whitespace;

fn debian_ws<'a>() -> impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> + Clone {
    inline_whitespace().then_ignore(
        text::newline()
            .repeated()
            .exactly(1)
            .then(inline_whitespace().at_least(1))
            .or_not(),
    )
}

fn r_trivia<'a>() -> impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> + Clone {
    choice((
        // match comments
        just('#')
            .then(any().and_is(text::newline().not()).repeated())
            .to(())
            .padded_by(debian_ws())
            .repeated()
            .at_least(1),
        debian_ws(),
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RIdentifier<'a>(Cow<'a, str>);

impl<'a> RIdentifier<'a> {
    pub fn borrowed(v: &'a str) -> Self {
        Self(Cow::Borrowed(v))
    }
    pub fn owned(v: String) -> Self {
        Self(Cow::Owned(v))
    }
}

impl<'a> RIdentifier<'a> {
    pub fn into_rstr(self) -> RString<'a> {
        RString(self.0)
    }
}

fn parse_r_ident<'a>(
) -> impl Parser<'a, &'a str, RIdentifier<'a>, extra::Err<Rich<'a, char>>> + Clone {
    text::ident()
        .separated_by(just('.'))
        .allow_leading()
        .allow_trailing()
        .to_slice()
        .map(Cow::Borrowed)
        .map(RIdentifier)
}

// thinking, should we just handle
// escaping and then use a cow a la serde
// or is it best to keep it al la carte?
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RString<'a>(pub Cow<'a, str>);

impl<'a> RString<'a> {
    pub fn borrowed(v: &'a str) -> Self {
        Self(Cow::Borrowed(v))
    }
    pub fn owned(v: String) -> Self {
        Self(Cow::Owned(v))
    }
    pub fn to_static(&self) -> RString<'static> {
        match &self.0 {
            Cow::Borrowed(b) => RString(Cow::Owned((*b).to_owned())),
            Cow::Owned(o) => RString(Cow::Owned(o.clone())),
        }
    }
}
impl<'a> RString<'a> {
    pub fn to_rust_string(&self) -> String {
        match &self.0 {
            Cow::Borrowed(b) => (*b).to_owned(),
            Cow::Owned(o) => o.clone(),
        }
    }
    pub fn into_rust_string(self) -> String {
        match self.0 {
            Cow::Borrowed(b) => (*b).to_owned(),
            Cow::Owned(o) => o,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionArg<'a> {
    Named {
        name: RString<'a>,
        value: RAbstractSyntaxTree<'a>,
    },
    Unnamed {
        value: RAbstractSyntaxTree<'a>,
    },
}
fn parse_r_function_arg<
    'a,
    P: Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone,
>(
    p: P,
) -> impl Parser<'a, &'a str, FunctionArg<'a>, extra::Err<Rich<'a, char>>> + Clone {
    choice((parse_string(), parse_r_ident().map(RIdentifier::into_rstr)))
        .padded_by(debian_ws())
        .then_ignore(just('=').padded_by(debian_ws()))
        .or_not()
        .then(p.padded_by(debian_ws()))
        .map(|(name, value)| match name {
            Some(name) => FunctionArg::Named { name, value },
            None => FunctionArg::Unnamed { value },
        })
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgs<'a>(Vec<FunctionArg<'a>>);

fn parse_r_function_args<
    'a,
    P: Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone,
>(
    p: P,
) -> impl Parser<'a, &'a str, FunctionArgs<'a>, extra::Err<Rich<'a, char>>> + Clone {
    parse_r_function_arg(p)
        .separated_by(just(',').padded_by(r_trivia()))
        .collect::<Vec<_>>()
        .map(FunctionArgs)
}

fn parse_r_function<
    'a,
    P: Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone,
>(
    p: P,
) -> impl Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone {
    parse_r_ident()
        .then(parse_r_function_args(p).delimited_by(
            just('(').padded_by(r_trivia()),
            just(')').padded_by(r_trivia()),
        ))
        .map(|(name, args)| RAbstractSyntaxTree::FunctionCall { name, args })
}

#[derive(Debug, Clone, PartialEq)]
pub enum RAbstractSyntaxTree<'a> {
    FunctionCall {
        name: RIdentifier<'a>,
        args: FunctionArgs<'a>,
    },
    NA,
    Integer {
        value: i64,
    },
    Float {
        value: f64,
    },
    Null,
    Identifier(RIdentifier<'a>),
    String(RString<'a>),
}

pub fn parse_r_syntax_tree<'a>(
) -> impl Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|t| {
        parse_r_function(t)
            .or(parse_null())
            .or(parse_string().map(RAbstractSyntaxTree::String))
            .or(parse_float())
            .or(parse_integer())
            .or(parse_r_ident().map(RAbstractSyntaxTree::Identifier))
    })
}

fn parse_null<'a>(
) -> impl Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone {
    just("NULL").to(RAbstractSyntaxTree::Null)
}

fn parse_integer<'a>(
) -> impl Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone {
    just('-')
        .or_not()
        .then(text::int(10))
        .to(())
        .to_slice()
        .then_ignore(just('L').or_not())
        .map(|v: &str| RAbstractSyntaxTree::Integer {
            value: i64::from_str_radix(v, 10).unwrap(),
        })
}

fn parse_float<'a>(
) -> impl Parser<'a, &'a str, RAbstractSyntaxTree<'a>, extra::Err<Rich<'a, char>>> + Clone {
    let standard_float = (just('-').or_not().then(text::digits(10)))
        .then(just('.'))
        .then(just('-').or_not().then(text::digits(10)))
        .to(());

    let scientific_float = (just('-').or_not().then(text::digits(10)))
        .then(just('.').then(just('-').then(text::digits(10))).or_not())
        .then_ignore(just('e'))
        .then(just('-').or_not().then(text::digits(10)))
        .to(());

    scientific_float
        .or(standard_float)
        .to_slice()
        .map(|v: &str| RAbstractSyntaxTree::Float {
            value: v.parse().unwrap(),
        })
}

fn parse_string<'a>() -> impl Parser<'a, &'a str, RString<'a>, extra::Err<Rich<'a, char>>> + Clone {
    // if a string opens with a certain character, then it must end with the same character,
    // we could solve this a multitude of ways, but I think the simplest is probably to
    // just duplicate the logic twice, and or them, probably not the most efficient
    just('"')
        .ignore_then(
            choice((just('\\').then(any()).to(()), none_of('"').to(())))
                .repeated()
                .to_slice(),
        )
        .then_ignore(just('"'))
        .or(just('\'')
            .ignore_then(
                choice((just('\\').then(any()).to(()), none_of('\'').to(())))
                    .repeated()
                    .to_slice(),
            )
            .then_ignore(just('\'')))
        .map(Cow::Borrowed)
        .map(RString)
}

#[derive(Debug, Clone, PartialEq)]
pub struct RList<'a> {
    names: Vec<Option<RString<'a>>>,
    values: Vec<RValue<'a>>,
}
impl<'a> RList<'a> {
    fn from_vec(vec: Vec<RValue<'a>>) -> RList<'a> {
        Self {
            names: vec![None; vec.len()],
            values: vec,
        }
    }

    pub fn values(&self) -> &[RValue<'a>] {
        &self.values
    }

    pub fn get(&self, key: &str) -> Option<&RValue<'a>> {
        self.names.iter().enumerate().find_map(|(i, name)| {
            if let Some(RString(name)) = name {
                if name == key {
                    Some(&self.values[i])
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub enum RVecBackingDisc {
    RBool,
    RInt,
    RDouble,
    RStr,
    RList,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RVecBacking<'a> {
    // there's technically also complex
    // and bytes, but I'm not implementing
    // those more niche ones right now.
    RInt(Vec<i64>),
    RDouble(Vec<f64>),
    RBool(Vec<bool>),
    RStr(Vec<RString<'a>>),
    RList(Vec<RValue<'a>>),
}

impl<'a> RVecBacking<'a> {
    pub fn len(&self) -> usize {
        match self {
            RVecBacking::RInt(i) => i.len(),
            RVecBacking::RDouble(i) => i.len(),
            RVecBacking::RBool(i) => i.len(),
            RVecBacking::RStr(i) => i.len(),
            RVecBacking::RList(i) => i.len(),
        }
    }
    pub fn promote(self, disc: RVecBackingDisc) -> RVecBacking<'a> {
        // we note that promotion (->) isn't always transitive
        // we do not have: a -> b -> c <==> a -> c because
        // boolean -> integer -> string <=/=> boolean -> string,
        // a counterexample of which is TRUE -> 1 -> "1" <=/=> TRUE -> "TRUE"
        match (self, disc) {
            (id @ RVecBacking::RInt(_), RVecBackingDisc::RInt) => id,
            (RVecBacking::RInt(contents), RVecBackingDisc::RDouble) => {
                RVecBacking::RDouble(contents.into_iter().map(|t| t as f64).collect())
            }
            (RVecBacking::RInt(i), RVecBackingDisc::RStr) => RVecBacking::RStr(
                i.into_iter()
                    .map(|t| RString(t.to_string().into()))
                    .collect(),
            ),

            (id @ RVecBacking::RDouble(_), RVecBackingDisc::RDouble) => id,
            (RVecBacking::RDouble(d), RVecBackingDisc::RStr) => RVecBacking::RStr(
                d.into_iter()
                    .map(|t| RString(t.to_string().into()))
                    .collect(),
            ),
            (id @ RVecBacking::RBool(_), RVecBackingDisc::RBool) => id,
            (RVecBacking::RBool(d), RVecBackingDisc::RInt) => {
                RVecBacking::RInt(d.into_iter().map(|t| if t { 1 } else { 0 }).collect())
            }
            (RVecBacking::RBool(d), RVecBackingDisc::RDouble) => {
                RVecBacking::RDouble(d.into_iter().map(|t| if t { 1.0 } else { 0.0 }).collect())
            }
            (RVecBacking::RBool(d), RVecBackingDisc::RStr) => RVecBacking::RStr(
                d.into_iter()
                    .map(|t| if t { "TRUE" } else { "FALSE" })
                    .map(Cow::Borrowed)
                    .map(RString)
                    .collect(),
            ),

            (id @ RVecBacking::RStr(_), RVecBackingDisc::RStr) => id,

            (RVecBacking::RStr(lower), RVecBackingDisc::RList) => {
                RVecBacking::RList(lower.into_iter().map(RValue::Str).collect())
            }
            (RVecBacking::RBool(lower), RVecBackingDisc::RList) => {
                RVecBacking::RList(lower.into_iter().map(RValue::Bool).collect())
            }
            (RVecBacking::RInt(lower), RVecBackingDisc::RList) => {
                RVecBacking::RList(lower.into_iter().map(RValue::Int).collect())
            }
            (RVecBacking::RDouble(lower), RVecBackingDisc::RList) => {
                RVecBacking::RList(lower.into_iter().map(RValue::Double).collect())
            }
            (id @ RVecBacking::RList(_), RVecBackingDisc::RList) => id,
            (f, t) => {
                panic!("[IIE@{}]: cannot promote {:?} to {:?}", line!(), f, t)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RVec<'a> {
    pub names: Vec<Option<RString<'a>>>,
    pub attributes: HashMap<RString<'a>, RValue<'a>>,
    pub not_available: Vec<bool>,
    pub backing: RVecBacking<'a>,
}

impl<'a> RVec<'a> {
    pub fn iter_wrapped<F: Fn(RValue<'a>)>(&self, f: F) {
        match &self.backing {
            RVecBacking::RInt(is) => {
                for i in is {
                    f(RValue::Int(*i))
                }
            }
            RVecBacking::RDouble(ds) => {
                for i in ds {
                    f(RValue::Double(*i))
                }
            }
            RVecBacking::RBool(ds) => {
                for i in ds {
                    f(RValue::Bool(*i))
                }
            }
            RVecBacking::RStr(ds) => {
                for i in ds {
                    f(RValue::Str(i.clone()))
                }
            }
            RVecBacking::RList(rl) => todo!(),
        }
    }
    pub fn lift_str(value: RString<'a>) -> RVec<'a> {
        RVec {
            names: vec![None],
            attributes: HashMap::new(),
            not_available: vec![false],
            backing: RVecBacking::RStr(vec![value]),
        }
    }
    pub fn lift_int(value: i64) -> RVec<'a> {
        RVec {
            names: vec![None],
            attributes: HashMap::new(),
            not_available: vec![false],
            backing: RVecBacking::RInt(vec![value]),
        }
    }
    pub fn lift_double(value: f64) -> RVec<'a> {
        RVec {
            names: vec![None],
            attributes: HashMap::new(),
            not_available: vec![false],
            backing: RVecBacking::RDouble(vec![value]),
        }
    }
    pub fn lift_bool(value: bool) -> RVec<'a> {
        RVec {
            names: vec![None],
            attributes: HashMap::new(),
            not_available: vec![false],
            backing: RVecBacking::RBool(vec![value]),
        }
    }

    pub fn promote(self, disc: RVecBackingDisc) -> Self {
        Self {
            names: self.names,
            attributes: self.attributes,
            not_available: self.not_available,
            backing: self.backing.promote(disc),
        }
    }

    pub fn backing_type(item: &RValue<'a>) -> Option<RVecBackingDisc> {
        match item {
            RValue::Null => None,
            RValue::NA => Some(RVecBackingDisc::RBool),
            RValue::Bool(_) => Some(RVecBackingDisc::RBool),
            RValue::Double(_) => Some(RVecBackingDisc::RDouble),
            RValue::Int(_) => Some(RVecBackingDisc::RInt),
            RValue::Str(_) => Some(RVecBackingDisc::RStr),
            RValue::RVec(RVec {
                backing: RVecBacking::RBool(_),
                ..
            }) => Some(RVecBackingDisc::RBool),
            RValue::RVec(RVec {
                backing: RVecBacking::RInt(_),
                ..
            }) => Some(RVecBackingDisc::RInt),
            RValue::RVec(RVec {
                backing: RVecBacking::RDouble(_),
                ..
            }) => Some(RVecBackingDisc::RDouble),
            RValue::RVec(RVec {
                backing: RVecBacking::RStr(_),
                ..
            }) => Some(RVecBackingDisc::RStr),
            RValue::RVec(RVec {
                backing: RVecBacking::RList(_),
                ..
            }) => Some(RVecBackingDisc::RList),
        }
    }

    pub fn c(names: Vec<Option<RString<'a>>>, vec: Vec<RValue<'a>>) -> RValue<'a> {
        let Some(backing_type) = vec.iter().flat_map(Self::backing_type).max() else {
            return RValue::Null;
        };

        dbg!(backing_type);

        let names = names.into_iter().chain(std::iter::repeat(None));
        let mut out_names = Vec::new();
        let mut not_available = Vec::new();
        let backing = match backing_type {
            RVecBackingDisc::RBool => {
                let mut backing = Vec::<bool>::new();

                for (name, value) in names.zip(vec) {
                    match value.promote(backing_type) {
                        RValue::Null => {}
                        RValue::NA => unreachable!(),
                        RValue::Bool(b) => {
                            not_available.push(false);
                            out_names.push(name);
                            backing.push(b)
                        }
                        RValue::Double(_) => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(_) => unreachable!(),
                        RValue::RVec(RVec {
                            names,
                            attributes: _,
                            not_available: na,
                            backing: RVecBacking::RBool(bools),
                        }) => {
                            for (i, subname) in
                                names.into_iter().enumerate().map(|(i, x)| (i + 1, x))
                            {
                                let out_name = match (&name, subname) {
                                    (None, None) => None,
                                    (None, Some(sub)) => Some(sub),
                                    (Some(pref), None) => {
                                        Some(RString(Cow::Owned(format!("{}{}", pref.0, i))))
                                    }
                                    (Some(pref), Some(suff)) => {
                                        Some(RString(Cow::Owned(format!("{}.{}", pref.0, suff.0))))
                                    }
                                };
                                out_names.push(out_name);
                            }
                            not_available.extend(na);
                            backing.extend(bools);
                        }
                        RValue::RVec(_) => unreachable!(),
                    }
                }

                RVecBacking::RBool(backing)
            }
            RVecBackingDisc::RInt => {
                let mut backing = Vec::<i64>::new();
                for (name, value) in names.zip(vec) {
                    match value.promote(backing_type) {
                        RValue::Null => {}
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(_) => unreachable!(),
                        RValue::NA => unreachable!(),
                        RValue::Int(i) => {
                            not_available.push(false);
                            out_names.push(name);
                            backing.push(i)
                        }
                        RValue::Str(_) => unreachable!(),
                        RValue::RVec(RVec {
                            names,
                            attributes: _,
                            not_available: na,
                            backing: RVecBacking::RInt(bools),
                        }) => {
                            for (i, subname) in
                                names.into_iter().enumerate().map(|(i, x)| (i + 1, x))
                            {
                                let out_name = match (&name, subname) {
                                    (None, None) => None,
                                    (None, Some(sub)) => Some(sub),
                                    (Some(pref), None) => {
                                        Some(RString(Cow::Owned(format!("{}{}", pref.0, i))))
                                    }
                                    (Some(pref), Some(suff)) => {
                                        Some(RString(Cow::Owned(format!("{}.{}", pref.0, suff.0))))
                                    }
                                };
                                out_names.push(out_name);
                            }
                            not_available.extend(na);
                            backing.extend(bools);
                        }
                        RValue::RVec(_) => unreachable!(),
                    }
                }

                RVecBacking::RInt(backing)
            }
            RVecBackingDisc::RDouble => {
                let mut backing = Vec::<f64>::new();
                for (name, value) in names.zip(vec) {
                    match value.promote(backing_type) {
                        RValue::Null => {}
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(d) => {
                            not_available.push(false);
                            out_names.push(name);
                            backing.push(d)
                        }
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(_) => unreachable!(),
                        RValue::NA => unreachable!(),
                        RValue::RVec(RVec {
                            names,
                            attributes: _,
                            not_available: na,
                            backing: RVecBacking::RDouble(doubles),
                        }) => {
                            for (i, subname) in
                                names.into_iter().enumerate().map(|(i, x)| (i + 1, x))
                            {
                                let out_name = match (&name, subname) {
                                    (None, None) => None,
                                    (None, Some(sub)) => Some(sub),
                                    (Some(pref), None) => {
                                        Some(RString(Cow::Owned(format!("{}{}", pref.0, i))))
                                    }
                                    (Some(pref), Some(suff)) => {
                                        Some(RString(Cow::Owned(format!("{}.{}", pref.0, suff.0))))
                                    }
                                };
                                out_names.push(out_name);
                            }
                            not_available.extend(na);
                            backing.extend(doubles);
                        }
                        RValue::RVec(_) => unreachable!(),
                    }
                }

                RVecBacking::RDouble(backing)
            }
            RVecBackingDisc::RStr => {
                let mut backing = Vec::<RString>::new();

                for (name, value) in names.zip(vec) {
                    match value.promote(backing_type) {
                        RValue::Null => {}
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(_) => unreachable!(),
                        RValue::NA => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(str) => {
                            not_available.push(false);
                            out_names.push(name);
                            backing.push(str)
                        }
                        RValue::RVec(RVec {
                            names,
                            attributes: _,
                            not_available: na,
                            backing: RVecBacking::RStr(strs),
                        }) => {
                            for (i, subname) in
                                names.into_iter().enumerate().map(|(i, x)| (i + 1, x))
                            {
                                let out_name = match (&name, subname) {
                                    (None, None) => None,
                                    (None, Some(sub)) => Some(sub),
                                    (Some(pref), None) => {
                                        Some(RString(Cow::Owned(format!("{}{}", pref.0, i))))
                                    }
                                    (Some(pref), Some(suff)) => {
                                        Some(RString(Cow::Owned(format!("{}.{}", pref.0, suff.0))))
                                    }
                                };
                                out_names.push(out_name);
                            }
                            not_available.extend(na);
                            backing.extend(strs);
                        }
                        RValue::RVec(_) => unreachable!(),
                    }
                }

                RVecBacking::RStr(backing)
            }
            RVecBackingDisc::RList => {
                let mut backing = Vec::<RValue>::new();
                for (name, value) in names.zip(vec) {
                    match value.promote(backing_type) {
                        RValue::Null => {}
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(_) => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(_) => unreachable!(),
                        RValue::NA => unreachable!(),
                        RValue::RVec(RVec {
                            names,
                            attributes: _,
                            not_available: na,
                            backing: RVecBacking::RList(strs),
                        }) => {
                            for (i, subname) in
                                names.into_iter().enumerate().map(|(i, x)| (i + 1, x))
                            {
                                let out_name = match (&name, subname) {
                                    (None, None) => None,
                                    (None, Some(sub)) => Some(sub),
                                    (Some(pref), None) => {
                                        Some(RString(Cow::Owned(format!("{}{}", pref.0, i))))
                                    }
                                    (Some(pref), Some(suff)) => {
                                        Some(RString(Cow::Owned(format!("{}.{}", pref.0, suff.0))))
                                    }
                                };
                                out_names.push(out_name);
                            }
                            not_available.extend(na);
                            backing.extend(strs);
                        }
                        RValue::RVec(_) => unreachable!(),
                    }
                }

                RVecBacking::RList(backing)
            }
        };

        assert_eq!(out_names.len(), not_available.len());
        assert_eq!(not_available.len(), backing.len());
        RValue::RVec(RVec {
            names: out_names,
            attributes: HashMap::new(),
            not_available,
            backing,
        })
    }
    pub fn from_names_vec_value(
        names: Vec<Option<RString<'a>>>,
        mut vec: Vec<RValue<'a>>,
    ) -> RVec<'a> {
        // what we really want to do is take
        // the vec, and auto-promote it to the
        // correct data type, if that is possible.

        let mut best_dtype = RVecBackingDisc::RBool;

        let mut i = 0;

        while i < vec.len() {
            match &vec[i] {
                RValue::Null => {
                    vec.remove(i);
                    continue;
                }
                RValue::NA => {}
                RValue::Bool(_) => best_dtype = best_dtype.max(RVecBackingDisc::RBool),
                RValue::Double(_) => best_dtype = best_dtype.max(RVecBackingDisc::RDouble),
                RValue::Int(_) => best_dtype = best_dtype.max(RVecBackingDisc::RInt),
                RValue::Str(_) => best_dtype = best_dtype.max(RVecBackingDisc::RStr),
                RValue::RVec(_) => best_dtype = best_dtype.max(RVecBackingDisc::RList),
            }

            i += 1;
        }

        let in_vec_len = vec.len();

        let vec = vec
            .into_iter()
            .flat_map(|t| {
                if let RValue::Null = t {
                    None
                } else {
                    Some(t.promote(best_dtype))
                }
            })
            .collect::<Vec<_>>();

        let backing = match best_dtype {
            RVecBackingDisc::RBool => {
                let mut backing = Vec::<bool>::with_capacity(in_vec_len);
                for value in vec {
                    match value {
                        RValue::Null => {}
                        RValue::NA => unreachable!(),
                        RValue::Bool(b) => backing.push(b),
                        RValue::Double(_) => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(_) => unreachable!(),
                        RValue::RVec(_) => unreachable!(),
                    }
                }
                RVecBacking::RBool(backing)
            }
            RVecBackingDisc::RInt => {
                let mut backing = Vec::<i64>::with_capacity(in_vec_len);
                for value in vec {
                    match value {
                        RValue::Null => todo!(),
                        RValue::NA => todo!(),
                        RValue::Bool(_) => todo!(),
                        RValue::Double(_) => todo!(),
                        RValue::Int(i) => backing.push(i),
                        RValue::Str(_) => todo!(),
                        RValue::RVec(_) => todo!(),
                    }
                }
                RVecBacking::RInt(backing)
            }
            RVecBackingDisc::RDouble => {
                let mut backing = Vec::<f64>::with_capacity(in_vec_len);
                for value in vec {
                    match value {
                        RValue::Null => unreachable!(),
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(d) => backing.push(d),
                        RValue::NA => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::Str(_) => unreachable!(),
                        RValue::RVec(_) => unreachable!(),
                    }
                }
                RVecBacking::RDouble(backing)
            }
            RVecBackingDisc::RStr => {
                let mut backing = Vec::<RString>::with_capacity(in_vec_len);
                for value in vec {
                    match value {
                        RValue::Null => unreachable!(),
                        RValue::Bool(_) => unreachable!(),
                        RValue::Double(_) => unreachable!(),
                        RValue::Int(_) => unreachable!(),
                        RValue::NA => unreachable!(),
                        RValue::Str(str) => backing.push(str),
                        RValue::RVec(_) => unreachable!(),
                    }
                }
                RVecBacking::RStr(backing)
            }
            RVecBackingDisc::RList => todo!(),
        };

        Self {
            names,
            attributes: HashMap::new(),
            not_available: vec![false; in_vec_len],
            backing,
        }
    }

    /// always creates with a backing type of list
    fn list(names: Vec<Option<RString<'a>>>, values: Vec<RValue<'a>>) -> RVec<'a> {
        RVec {
            names,
            attributes: {
                let mut map = HashMap::new();
                map.insert(
                    RString::borrowed("class"),
                    RValue::RVec(RVec::lift_str(RString::borrowed("list"))),
                );
                map
            },
            not_available: vec![false; values.len()],
            backing: RVecBacking::RList(values),
        }
    }

    fn with_class(self, class: &'a str) -> Self {
        Self {
            names: self.names,
            attributes: {
                let mut map = self.attributes;

                match map.get_mut(&RString::borrowed("class")) {
                    Some(RValue::RVec(RVec {
                        backing: RVecBacking::RStr(strs),
                        ..
                    })) => {
                        strs.push(RString::borrowed(class));
                    }
                    _ => {
                        map.insert(
                            RString::borrowed("class"),
                            RValue::RVec(RVec::lift_str(RString::borrowed(class))),
                        );
                    }
                };

                map
            },
            not_available: self.not_available,
            backing: self.backing,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RValue<'a> {
    Null,
    NA,
    Bool(bool),
    Double(f64),
    Int(i64),
    Str(RString<'a>),
    RVec(RVec<'a>),
}

impl<'a> RValue<'a> {
    pub fn promote(self, disc: RVecBackingDisc) -> Self {
        match (self, disc) {
            (id @ RValue::Int(_), RVecBackingDisc::RInt) => id,
            (RValue::Int(contents), RVecBackingDisc::RDouble) => RValue::Double(contents as f64),
            (RValue::Int(i), RVecBackingDisc::RStr) => RValue::Str(RString(i.to_string().into())),

            (id @ RValue::Double(_), RVecBackingDisc::RDouble) => id,
            (RValue::Double(d), RVecBackingDisc::RStr) => {
                RValue::Str(RString(d.to_string().into()))
            }
            (id @ RValue::Bool(_), RVecBackingDisc::RBool) => id,
            (RValue::Bool(t), RVecBackingDisc::RInt) => RValue::Int(if t { 1 } else { 0 }),
            (RValue::Bool(t), RVecBackingDisc::RDouble) => {
                RValue::Double(if t { 1.0 } else { 0.0 })
            }
            (RValue::Bool(t), RVecBackingDisc::RStr) => {
                RValue::Str(RString(Cow::Borrowed(if t { "TRUE" } else { "FALSE" })))
            }

            (id @ RValue::Str(_), RVecBackingDisc::RStr) => id,

            // for these, I think this is essentially a lift operation
            (RValue::Str(lower), RVecBackingDisc::RList) => RValue::RVec(RVec::lift_str(lower)),
            (RValue::Bool(lower), RVecBackingDisc::RList) => RValue::RVec(RVec::lift_bool(lower)),
            (RValue::Int(lower), RVecBackingDisc::RList) => RValue::RVec(RVec::lift_int(lower)),
            (RValue::Double(lower), RVecBackingDisc::RList) => {
                RValue::RVec(RVec::lift_double(lower))
            }

            (RValue::RVec(this), _) => RValue::RVec(this.promote(disc)),
            (f, t) => {
                panic!("[IIE@{}]: cannot promote {:?} to {:?}", line!(), f, t)
            }
        }
    }
}

struct RPerson<'a> {
    given: Option<&'a str>,
    family: Option<&'a str>,
    middle: Option<&'a str>,
    email: Option<&'a str>,
    role: Vec<&'a str>,
    comment: RValue<'a>,
    first: Option<&'a str>,
    last: Option<&'a str>,
}

struct FunctionCallMachine<'ag, 'a> {
    processed: HashMap<RString<'ag>, RValue<'a>>,
    // (name, default_value)
    still_to_process: Vec<(RString<'ag>, Option<RValue<'a>>)>,
}

impl<'ag, 'a> FunctionCallMachine<'ag, 'a> {
    pub fn new(args: Vec<(RString<'ag>, Option<RValue<'a>>)>) -> Self {
        Self {
            processed: HashMap::new(),
            still_to_process: args,
        }
    }
    pub fn satisfy(&mut self, value: RValue<'a>) {
        let (name, _) = self.still_to_process.remove(0);
        self.processed.insert(name, value);
    }

    pub fn satisfy_by_name(&mut self, name: &RString<'ag>, value: RValue<'a>) -> Result<(), ()> {
        let mut best_match = None;
        let mut best_match_len = 0;

        for (idx, (arg_name, _)) in self.still_to_process.iter().enumerate() {
            if arg_name == name {
                best_match = Some(idx);
            }

            let mut common = 0;

            // count number of prefixing characters.
            for (a, b) in arg_name.0.chars().zip(name.0.chars()) {
                if a == b {
                    common += 1;
                } else {
                    break;
                }
            }

            if common > best_match_len {
                best_match = Some(idx);
                best_match_len = common;
            }
        }

        if let Some(pop_idx) = best_match {
            let (name, _) = self.still_to_process.remove(pop_idx);
            self.processed.insert(name, value);
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn finalize(
        mut self,
    ) -> Result<HashMap<RString<'ag>, RValue<'a>>, Vec<(RString<'ag>, Option<RValue<'a>>)>> {
        // we need to pop off every value which has a
        // default and then we can see.

        let mut i = 0;
        while i < self.still_to_process.len() {
            if self.still_to_process[i].1.is_some() {
                let (name, value) = self.still_to_process.remove(i);
                self.processed.insert(name, value.unwrap());
            } else {
                i += 1;
            }
        }

        if self.still_to_process.is_empty() {
            Ok(self.processed)
        } else {
            Err(self.still_to_process)
        }
    }
}

pub fn interpret_r<'a>(ast: &RAbstractSyntaxTree<'a>) -> miette::Result<RValue<'a>> {
    match ast {
        RAbstractSyntaxTree::NA => Ok(RValue::NA),
        RAbstractSyntaxTree::FunctionCall { name, args } => {
            let RIdentifier(name) = name;
            let FunctionArgs(args) = args;

            match &**name {
                // the `c` function in R
                // creates a vector
                "c" => {
                    let mut names = vec![];
                    let mut values = vec![];

                    for arg in args {
                        match arg {
                            FunctionArg::Named { name, value } => {
                                names.push(Some(name.clone()));
                                values.push(interpret_r(&value)?)
                            }
                            FunctionArg::Unnamed { value } => {
                                names.push(None);
                                values.push(interpret_r(&value)?)
                            }
                        };
                    }
                    Ok(RVec::c(names, values))
                }
                "list" => {
                    let mut names = vec![];
                    let mut values = vec![];

                    for arg in args {
                        match arg {
                            FunctionArg::Named { name, value } => {
                                names.push(Some(name.clone()));
                                values.push(interpret_r(value)?)
                            }
                            FunctionArg::Unnamed { value } => {
                                names.push(None);
                                values.push(interpret_r(value)?)
                            }
                        };
                    }
                    Ok(RValue::RVec(RVec::list(names, values)))
                }
                "person" => {
                    // since this one is defined non-intrinsically
                    // we need to actually do a method dispatch, which
                    // defintitely makes things a little bit more complicated.

                    // let ident = |v| RString(Cow::Borrowed(v));

                    // let mut call_machine = FunctionCallMachine::new(vec![
                    //     (ident("given"), Some(RValue::Null)),
                    //     (ident("family"), Some(RValue::Null)),
                    //     (ident("middle"), Some(RValue::Null)),
                    //     (ident("email"), Some(RValue::Null)),
                    //     (ident("role"), Some(RValue::Null)),
                    //     (ident("comment"), Some(RValue::Null)),
                    //     (ident("first"), Some(RValue::Null)),
                    //     (ident("last"), Some(RValue::Null)),
                    // ]);

                    // let mut args_cloned = args.clone();

                    // args_cloned.sort_by_key(|k| matches!(k, FunctionArg::Unnamed { .. }));

                    // for arg in &args_cloned {
                    //     match arg {
                    //         FunctionArg::Named { name, value } => {
                    //             if let Err(_) =
                    //                 call_machine.satisfy_by_name(name, interpret_r(value)?)
                    //             {
                    //                 return Err(miette!(
                    //                     "failed to call function with argument named `{}`",
                    //                     name.0
                    //                 ));
                    //             }
                    //         }
                    //         FunctionArg::Unnamed { value } => {
                    //             call_machine.satisfy(interpret_r(value)?)
                    //         }
                    //     }
                    // }

                    // let mut res = match call_machine.finalize() {
                    //     Ok(ok) => {
                    //         ok
                    //     },
                    //     Err(err) => return Err(miette!("couldn't call the function because of the following arguments: {err:#?}")),
                    // };

                    // let mut names = Vec::new();
                    // let mut values = Vec::new();

                    // for iden in [
                    //     ident("given"),
                    //     ident("family"),
                    //     ident("middle"),
                    //     ident("email"),
                    //     ident("role"),
                    //     ident("comment"),
                    //     ident("first"),
                    //     ident("last"),
                    // ] {
                    //     let (name, value) = res.remove_entry(&iden).unwrap();
                    //     names.push(Some(name));
                    //     values.push(value);
                    // }
                    // dbg!(&values);
                    // Ok(RVec::list(names, values))
                    let person = rfunc_intrinsic::person_def();

                    let res = rfunc_intrinsic::call_intrinsic("person", &args[..], person)?;

                    Ok(res)
                }
                // I literally do not know why we would
                // ever want to do this while defining authors
                // but someone has done it, so we need to support it.
                "print" => {
                    let print = rfunc_intrinsic::rprint_def();

                    let res = rfunc_intrinsic::call_intrinsic("print", &args[..], print)?;

                    Ok(res)
                }
                "comment" => Ok(RValue::Null),
                "paste" => {
                    let mut res = String::new();
                    for (i, arg) in args.iter().enumerate() {
                        match arg {
                            FunctionArg::Named { name: _, value: _ } => {
                                return Err(miette!(
                                    "function `paste` does not support named arguments, yet",
                                ))
                            }
                            FunctionArg::Unnamed { value } => match interpret_r(value)? {
                                RValue::Str(RString(s)) => {
                                    res.push_str(&s);
                                    if i != args.len() - 1 {
                                        res.push_str(" ");
                                    }
                                }
                                _ => return Err(miette!("only strings are supported in paste")),
                            },
                        }
                    }
                    Ok(RValue::Str(RString(Cow::Owned(res))))
                }
                "paste0" => {
                    let mut res = String::new();
                    for arg in args {
                        match arg {
                            FunctionArg::Named { name: _, value: _ } => {
                                return Err(miette!(
                                    "function `paste` does not support named arguments, yet",
                                ))
                            }
                            FunctionArg::Unnamed { value } => match interpret_r(value)? {
                                RValue::Str(RString(s)) => {
                                    res.push_str(&s);
                                }
                                _ => return Err(miette!("only strings are supported in paste")),
                            },
                        }
                    }
                    Ok(RValue::Str(RString(Cow::Owned(res))))
                }
                // "structure" => {
                //     let ident = |v| RString(Cow::Borrowed(v));

                //     let call_machine = FunctionCallMachine::new(vec![
                //         (ident(".Data"), Some(RValue::Null)),
                //         (ident(".Names"), Some(RValue::Null)),
                //     ]);

                //     let mut res = call_r_func(&mut args.clone(), call_machine)?;

                //     let (names, values) =
                //         match (res.remove(&ident(".Data")), res.remove(&ident(".Names"))) {
                //             (None, None) => {
                //                 return Err(format!(
                //                     "structure function called without any arguments"
                //                 ))
                //             }
                //             (None, Some(_)) => {
                //                 return Err("structure function called without data".to_owned())
                //             }
                //             (Some(RValue::RList(RList { names: _, values })), None) => {
                //                 (vec![], values)
                //             }
                //             (Some(val), None) => (vec![], vec![val]),
                //             (Some(val), Some(names)) => {
                //                 let mut out_names = Vec::new();
                //                 let mut out_values = Vec::new();
                //                 match (names, val) {
                //                     (RValue::RList(names), RValue::RList(values)) => {
                //                         for name in names.values.iter() {
                //                             if let RValue::Str(rstr @ RString(_)) = name {
                //                                 out_names.push(Some(rstr.clone()));
                //                             } else {
                //                                 return Err(
                //                                 "structure function called with invalid arguments"
                //                                     .to_owned(),
                //                             );
                //                             }
                //                         }
                //                         out_values.extend(values.values);
                //                     }
                //                     (RValue::Str(str), RValue::RList(values)) => {
                //                         out_names.push(Some(str));
                //                         out_values.extend(values.values);
                //                     }
                //                     (RValue::Str(str), rval @ RValue::Str(_)) => {
                //                         out_names.push(Some(str));
                //                         out_values.push(rval);
                //                     }
                //                     _ => {
                //                         return Err(
                //                             "structure function called with invalid arguments"
                //                                 .to_owned(),
                //                         )
                //                     }
                //                 };
                //                 (out_names, out_values)
                //             }
                //         };
                //     Ok(RValue::RList(RList { names, values }))
                // }
                "" => match args.as_slice() {
                    [] => Err(miette::miette!("() is not valid value type instance in R")),
                    [FunctionArg::Unnamed { value }] => interpret_r(&value),
                    // it seems like R treats a set of paranthesis
                    // as implicitly as the `c` function if there is ambiguity
                    args @ [..] => {
                        let mut names = vec![];
                        let mut values = vec![];

                        for arg in args {
                            match arg {
                                FunctionArg::Named { name, value } => {
                                    names.push(Some(name.clone()));
                                    values.push(interpret_r(&value)?)
                                }
                                FunctionArg::Unnamed { value } => {
                                    names.push(None);
                                    values.push(interpret_r(&value)?)
                                }
                            };
                        }
                        Ok(RValue::RVec(RVec::from_names_vec_value(names, values)))
                    }
                },
                _ => Err(miette::miette!("function `{}` not implemented", name)),
            }
        }
        RAbstractSyntaxTree::Integer { value } => Ok(RValue::Int(*value)),
        RAbstractSyntaxTree::Float { value } => Ok(RValue::Double(*value)),
        RAbstractSyntaxTree::Identifier(RIdentifier(Cow::Borrowed(""))) => Ok(RValue::Null),
        RAbstractSyntaxTree::Identifier(RIdentifier(ident)) => {
            Err(miette::miette!("{ident} not defined yet!"))
        }
        RAbstractSyntaxTree::String(str) => Ok(RValue::Str(str.clone())),
        RAbstractSyntaxTree::Null => Ok(RValue::Null),
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use super::{
        parse_float, parse_integer, parse_string, r_trivia, RAbstractSyntaxTree, RString, RValue,
        RVec,
    };
    use chumsky::{
        error::Rich,
        extra,
        span::{SimpleSpan, Span},
        Parser,
    };
    use miette::{miette, LabeledSpan, Severity};

    use super::{interpret_r, parse_r_syntax_tree};

    #[derive(thiserror::Error, Debug, miette::Diagnostic)]
    #[error("R error!")]
    struct RError {}
    pub fn pretty_errors<'a>(
        filename: String,
        file: &'a str,
        errs: Vec<Rich<'a, char, SimpleSpan>>,
    ) {
        for e in errs.into_iter().map(|e| e.map_token(|c| c.to_string())) {
            let labels = LabeledSpan::at(e.span().start()..e.span().end(), e.reason().to_string());
            let report = miette!(
                // Those fields are optional
                severity = Severity::Error,
                labels = vec![labels],
                url = "https://example.com",
                // Rest of the arguments are passed to `format!`
                // to form diagnostic message
                "expected closing ')'"
            )
            .with_source_code(file.to_owned());

            println!("{:?}", report);

            // let miette_diag = miette!(
            //     labels = vec![LabeledSpan::at(12..13, "this should be 6")],
            //     e.to_string()
            // )
            // .with_source_code(file);

            // let report = Report::build(
            //     ReportKind::Error,
            //     (filename.clone(), e.span().start..e.span().end()),
            // )
            // .with_message(e.to_string())
            // .with_label(
            //     Label::new((filename.clone(), e.span().start()..e.span().end()))
            //         .with_message(e.reason().to_string())
            //         .with_color(Color::Red),
            // )
            // .with_labels(e.contexts().map(|(label, span)| {
            //     Label::new((filename.clone(), span.into_range()))
            //         .with_message(format!("while parsing this {}", label))
            //         .with_color(Color::Yellow)
            // }))
            // .finish();

            // report.print(sources([(filename.clone(), file)])).unwrap();
        }
    }

    fn pretty_parse<'a, 'b, V, P: Parser<'a, &'a str, V, extra::Err<Rich<'a, char>>>>(
        parser: P,
        input: &'a str,
        name: &'b str,
    ) -> miette::Result<V> {
        let res = parser.parse(input).into_result();

        match res {
            Ok(x) => Ok(x),
            Err(errs) => {
                pretty_errors(name.to_string(), input, errs);
                Err(miette!("failed to parse"))
            }
        }
    }

    #[test]
    pub fn test_ident() -> miette::Result<()> {
        use super::*;
        assert_eq!(
            pretty_parse(parse_r_ident(), "hello.world", "hello.world")?,
            RIdentifier(Cow::Borrowed("hello.world"))
        );
        assert_eq!(
            pretty_parse(parse_r_ident(), "data.frame", "data.frame")?,
            RIdentifier(Cow::Borrowed("data.frame"))
        );
        assert_eq!(
            pretty_parse(parse_r_ident(), ".Named", ".Named")?,
            RIdentifier(Cow::Borrowed(".Named"))
        );
        assert_eq!(
            pretty_parse(parse_r_ident(), "str", "str")?,
            RIdentifier(Cow::Borrowed("str"))
        );
        assert_eq!(
            pretty_parse(parse_r_ident(), "str_str", "str_str")?,
            RIdentifier(Cow::Borrowed("str_str"))
        );
        Ok(())
    }

    #[test]
    pub fn test_function_thing() -> miette::Result<()> {
        use super::*;

        use FunctionArg::*;
        use RAbstractSyntaxTree::*;

        assert_eq!(
            pretty_parse(parse_r_syntax_tree(), "person(1, 2, 3)", "list parse")?,
            FunctionCall {
                name: RIdentifier(Cow::Borrowed("person")),
                args: FunctionArgs(vec![
                    Unnamed {
                        value: Integer { value: 1 }
                    },
                    Unnamed {
                        value: Integer { value: 2 }
                    },
                    Unnamed {
                        value: Integer { value: 3 }
                    },
                ])
            }
        );
        assert_eq!(
            pretty_parse(
                parse_r_syntax_tree(),
                "c(person(1, 2, 3, comment = c(ORCHID = \"34132141\")))",
                "list parse"
            )?,
            FunctionCall {
                name: RIdentifier(Cow::Borrowed("c")),
                args: FunctionArgs(vec![Unnamed {
                    value: FunctionCall {
                        name: RIdentifier(Cow::Borrowed("person")),
                        args: FunctionArgs(vec![
                            Unnamed {
                                value: Integer { value: 1 }
                            },
                            Unnamed {
                                value: Integer { value: 2 }
                            },
                            Unnamed {
                                value: Integer { value: 3 }
                            },
                            Named {
                                name: (RString(Cow::Borrowed("comment"))),
                                value: FunctionCall {
                                    name: RIdentifier(Cow::Borrowed("c")),
                                    args: FunctionArgs(vec![Named {
                                        name: RString(Cow::Borrowed("ORCHID")),
                                        value: String(RString(Cow::Borrowed("34132141")))
                                    }])
                                }
                            }
                        ])
                    }
                }])
            }
        );

        assert_eq!(
            pretty_parse(
                parse_r_syntax_tree(),
                "c(person(1, 2, 3, comment = c(
                \"ORCHID\" = \"34132141\")))",
                "list parse quotes"
            )?,
            FunctionCall {
                name: RIdentifier(Cow::Borrowed("c")),
                args: FunctionArgs(vec![Unnamed {
                    value: FunctionCall {
                        name: RIdentifier(Cow::Borrowed("person")),
                        args: FunctionArgs(vec![
                            Unnamed {
                                value: Integer { value: 1 }
                            },
                            Unnamed {
                                value: Integer { value: 2 }
                            },
                            Unnamed {
                                value: Integer { value: 3 }
                            },
                            Named {
                                name: RString(Cow::Borrowed("comment")),
                                value: FunctionCall {
                                    name: RIdentifier(Cow::Borrowed("c")),
                                    args: FunctionArgs(vec![Named {
                                        name: RString(Cow::Borrowed("ORCHID")),
                                        value: String(RString(Cow::Borrowed("34132141")))
                                    }])
                                }
                            }
                        ])
                    }
                }])
            }
        );

        Ok(())
    }

    #[test]
    pub fn test_parse_integer() -> miette::Result<()> {
        assert_eq!(
            pretty_parse(parse_integer(), "2", "parse digit")?,
            RAbstractSyntaxTree::Integer { value: 2 }
        );
        assert_eq!(
            pretty_parse(parse_integer(), "32", "parse 2 digits")?,
            RAbstractSyntaxTree::Integer { value: 32 }
        );
        assert_eq!(
            pretty_parse(parse_integer(), "-12", "parse negative number")?,
            RAbstractSyntaxTree::Integer { value: -12 }
        );
        Ok(())
    }

    #[test]
    pub fn test_parse_float() -> miette::Result<()> {
        assert_eq!(
            pretty_parse(parse_float(), "2.5", "parse float")?,
            RAbstractSyntaxTree::Float { value: 2.5 }
        );
        assert_eq!(
            pretty_parse(parse_float(), "32.00", "parse 2 digit float")?,
            RAbstractSyntaxTree::Float { value: 32.0 }
        );
        assert_eq!(
            pretty_parse(parse_float(), "-12.0", "parse negative float")?,
            RAbstractSyntaxTree::Float { value: -12.0 }
        );
        assert_eq!(
            pretty_parse(parse_float(), "-12e-19", "parse negative float")?,
            RAbstractSyntaxTree::Float { value: -12e-19 }
        );
        Ok(())
    }

    #[test]
    pub fn test_string() -> miette::Result<()> {
        assert_eq!(
            pretty_parse(parse_string(), r#""A""#, "parse simple string")?,
            RString(Cow::Borrowed("A"))
        );
        assert_eq!(
            pretty_parse(parse_string(), r#"'A'"#, "parse simple string")?,
            RString(Cow::Borrowed("A"))
        );
        assert_eq!(
            pretty_parse(
                parse_string(),
                r#""A much longer string""#,
                "parse longer string"
            )?,
            RString(Cow::Borrowed("A much longer string"))
        );
        assert_eq!(
            pretty_parse(
                parse_string(),
                r#""A much \" longer string""#,
                "parse longer string"
            )?,
            RString(Cow::Borrowed("A much \\\" longer string"))
        );
        Ok(())
    }

    #[test]
    fn test_err() -> miette::Result<()> {
        pretty_parse(parse_r_syntax_tree(), "c(1, 2, 3)", "there")?;
        Ok(())
    }

    #[test]
    fn test_r_trivia() -> miette::Result<()> {
        let input = r#"# these are all comments
    # just like this, and they are working well
    "#;
        dbg!(pretty_parse(r_trivia(), input, "real test")?);
        Ok(())
    }

    #[test]
    fn test_r_c() -> miette::Result<()> {
        dbg!(RVec::c(
            vec![None, None, Some(RString(Cow::Borrowed("a")))],
            vec![
                RValue::Int(2),
                RValue::Int(1),
                RVec::c(
                    vec![Some(RString(Cow::Borrowed("a")))],
                    vec![RValue::Int(1), RValue::Bool(false),]
                )
            ]
        ));
        // dbg!(RValue::RVec(RVec::c(
        //     vec![],
        //     vec![RValue::RVec(RVec::from_names_vec_value(
        //         vec![],
        //         vec![RValue::Bool(true)]
        //     ))]
        // )));
        Ok(())
    }
    #[test]
    fn test_r_interpret() -> miette::Result<()> {
        let input = r#"c(person("Mine", "Dogucu", , "mdogucu@gmail.com", c("aut", "cre"), comment = c(ORCID = "0000-0002-8007-934X")))"#;
        let parsed = pretty_parse(parse_r_syntax_tree(), input, "real test")?;
        let interpreted = interpret_r(&parsed)?;
        dbg!(interpreted);
        Ok(())
    }
    #[test]
    fn test_r_interpret_print() -> miette::Result<()> {
        // let input = r#"print(list(list(a = list(1.2, 2, c(3, 4))), list(list(1, 2, 3))))"#;
        let input = "print(c(a = 3, b = 4))";
        let parsed = pretty_parse(parse_r_syntax_tree(), input, "real test")?;
        let interpreted = interpret_r(&parsed)?;
        dbg!(interpreted);
        Ok(())
    }

    #[test]
    fn real_test() -> miette::Result<()> {
        let input = r#"print(c(
    person("Mine", "Dogucu", , "mdogucu@gmail.com", c("aut", "cre"), comment = c(ORCID = "0000-0002-8007-934X")),
    person("Alicia", "Johnson", , role = "aut"),
    # comment here
    person("Miles", "Ott", , role = "aut", comment = c(ORCID = "0000-0003-4457-6565"))
    ))"#;
        let res = pretty_parse(parse_r_syntax_tree(), input, "real test")?;

        let interpreted = interpret_r(&res)?;
        dbg!(interpreted);
        Ok(())
    }
}
