use std::collections::HashMap;

use miette::miette;

use crate::{interpret_r, FunctionArg, RString, RValue, RVec, RVecBacking};

pub fn call_intrinsic<'a>(
    name: &str,
    args: &[FunctionArg<'a>],
    def: RFunctionDef<'a>,
) -> miette::Result<RValue<'a>> {
    let rfa = from_name_and_args(name, def.signature, args)?;

    let res = (def.def)(rfa)?;
    Ok(res)
}

fn from_name_and_args<'a>(
    name: &str,
    sig: RFuncSignature<'a>,
    args: &[FunctionArg<'a>],
) -> Result<RFuncArgs<'a>, miette::Error> {
    let func_name = name;

    let mut names = Vec::<RString>::new();
    let mut func_args = Vec::new();
    let mut kwargs = if sig.has_kwargs {
        Some(HashMap::new())
    } else {
        None
    };

    let mut name_queue = sig.arg_names.iter().rev().collect::<Vec<_>>();

    for arg in args {
        match arg {
            FunctionArg::Named { name, value } => {
                // does any argument name match the name
                // of the current argument.

                let exists_in_names = names.iter().map(|i| &i.0).any(|v| v == &name.0);
                if sig.has_kwargs && !exists_in_names {
                    kwargs
                        .as_mut()
                        .unwrap()
                        .insert(name.clone(), interpret_r(value)?);
                    continue;
                } else if !sig.arg_names.iter().map(|i| &i.0).any(|v| v == &name.0) {
                    return Err(miette!(
                        "error calling function {func_name}: {func_name} does not have argument {}",
                        name.0
                    ));
                }

                // don't allow repeating arguments,
                // that would be bad.
                if names.iter().map(|i| &i.0).any(|v| v == &name.0) {
                    return Err(miette!(
                        "error calling function {func_name}: {func_name} has duplicate argument {}",
                        name.0
                    ));
                }

                name_queue.retain(|n| &**n != name);

                names.push(name.clone());
                func_args.push(interpret_r(value)?);
            }
            FunctionArg::Unnamed { value } => {
                if name_queue.is_empty() {
                    return Err(miette!("error calling {func_name}: too many arguments"));
                }

                let name = name_queue.pop().unwrap();
                names.push(name.clone());
                func_args.push(interpret_r(value)?);
            }
        }
    }

    {
        let mut i = 0;
        while i < name_queue.len() {
            let name = &name_queue[i];

            let pos = sig.arg_names.iter().position(|i| &i == name);
            let default_val = pos.and_then(|p| sig.arg_defaults[p].clone());

            match default_val {
                Some(v) => {
                    names.push((*name).to_owned());
                    func_args.push(v);
                    name_queue.remove(i);
                }
                None => {
                    i += 1;
                }
            }
        }
    }
    if !name_queue.is_empty() {
        return Err(miette!(
            "error calling {func_name}: missing arguments: {missing}",
            missing = name_queue
                .iter()
                .map(|i| &*i.0)
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    if names.len() != func_args.len() {
        return Err(miette!(
                "error calling {func_name}: internal error: names and args are not the same length ({:?} vs {:?})", names, func_args
            ));
    }

    Result::<_, miette::Error>::Ok(RFuncArgs {
        names,
        func_args,
        kwargs,
    })
}

#[derive(Debug)]
pub struct RFuncArgs<'a> {
    names: Vec<RString<'a>>,
    func_args: Vec<RValue<'a>>,
    // certain functions have kwargs, some don't,
    // so we want to have those work correctly.
    kwargs: Option<HashMap<RString<'a>, RValue<'a>>>,
}

impl<'a> RFuncArgs<'a> {}

pub struct RFuncSignature<'a> {
    arg_names: Vec<RString<'a>>,
    arg_defaults: Vec<Option<RValue<'a>>>,
    has_kwargs: bool,
}

pub struct RFunctionDef<'a> {
    pub signature: RFuncSignature<'a>,
    pub def: for<'x> fn(RFuncArgs<'x>) -> miette::Result<RValue<'x>>,
}

pub fn person_def() -> RFunctionDef<'static> {
    RFunctionDef {
        signature: RFuncSignature {
            arg_names: [
                "given", "family", "middle", "email", "role", "comment", "first", "last",
            ]
            .map(RString::borrowed)
            .to_vec(),
            arg_defaults: vec![Some(RValue::Null); 8],
            has_kwargs: false,
        },
        def: |rfa| {
            let person = RVec::list(
                rfa.names.into_iter().map(|i| Some(i)).collect::<Vec<_>>(),
                rfa.func_args,
            )
            .with_class("person");

            Ok(RValue::RVec(person))
        },
    }
}

pub fn rprint_def() -> RFunctionDef<'static> {
    RFunctionDef {
        signature: RFuncSignature {
            arg_names: vec![RString::borrowed("x")],
            arg_defaults: vec![None],
            // this isn't *technically* correct in R,
            // print actually does allow so many keyword
            // arguments in print.default, and the S3 has
            // kwargs.
            has_kwargs: false,
        },
        def: |args| {
            let x = args.func_args.get(0).unwrap();
            print_r_item("", x);
            Ok(RValue::Null)
        },
    }
}

fn index_wrap(items: Vec<String>, wrap_at: usize, joiner: &str) {
    // print in R style, where, we
    // work to get up to `wrap_at` characters

    let mut buf = Vec::new();
    let mut current_line = Vec::new();
    let mut index_numbers = Vec::new();

    let mut current_width = 0;
    index_numbers.push(1);
    for (i, item) in items.into_iter().enumerate() {
        if item.len() + current_width > wrap_at {
            buf.push(std::mem::take(&mut current_line));
            index_numbers.push(i + 1);
            current_width = 0;
        }
        current_width += item.len() + joiner.len();
        current_line.push(item);
    }

    buf.push(current_line);

    let most_digits = index_numbers.last().unwrap().checked_ilog10().unwrap() + 1;
    for (i, line) in index_numbers.into_iter().zip(buf) {
        let current_digits = i.checked_ilog10().unwrap() + 1;
        // dbg!((most_digits, current_digits));

        println!(
            "{}[{i}] {}",
            " ".repeat((most_digits - current_digits) as usize),
            line.join(joiner),
        );
    }
}

pub fn print_r_item(prefix: &str, item: &RValue) {
    match item {
        RValue::Null => {
            println!("NULL");
        }
        RValue::NA => {
            println!("[1] NA");
        }
        RValue::Bool(true) => println!("[1] TRUE"),
        RValue::Bool(false) => println!("[1] FALSE"),
        RValue::Double(d) => println!("[1] {}", d),
        RValue::Int(i) => println!("[1] {}", i),
        RValue::Str(str) => println!("[1] \"{}\"", str.0),
        RValue::RVec(v) => print_r_vec(prefix, &v),
    }
}

pub fn print_r_vec(prefix: &str, rvec: &RVec) {
    match &rvec.backing {
        RVecBacking::RInt(i) => {
            let res = i
                .into_iter()
                .zip(&rvec.not_available)
                .map(|(v, na)| if *na { "NA".to_owned() } else { v.to_string() })
                .collect::<Vec<String>>();
            index_wrap(res, 80, " ");
        }

        RVecBacking::RDouble(i) => {
            let res = i
                .into_iter()
                .zip(&rvec.not_available)
                .map(|(v, na)| if *na { "NA".to_owned() } else { v.to_string() })
                .collect::<Vec<String>>();
            index_wrap(res, 80, " ");
        }
        RVecBacking::RBool(_) => todo!(),
        RVecBacking::RStr(v) => {
            let res = v
                .into_iter()
                .zip(&rvec.not_available)
                .map(|(v, na)| {
                    if *na {
                        "NA".to_owned()
                    } else {
                        v.0.to_string()
                    }
                })
                .collect::<Vec<String>>();
            index_wrap(res, 80, " ");
        }
        RVecBacking::RList(list) => {
            for (i, (v, n)) in list
                .iter()
                .zip(&rvec.names)
                .enumerate()
                .map(|(i, v)| (i + 1, v))
            {
                let leading = format!(
                    "{prefix}{}",
                    match n {
                        Some(n) => format!("${}", n.to_owned().0),
                        None => format!("[[{i}]]"),
                    }
                );
                println!("{leading}");
                print_r_item(&leading, v);
                println!();
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::index_wrap;

    #[test]
    fn test_index_wrap() {
        index_wrap(vec!["there".to_owned(); 400], 80, " ");
    }
}
