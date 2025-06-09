use std::{fs::File, io::Read, path::Path};

use either::Either;
use tyml::{header::TymlHeader, TymlContext};
use tyml_diagnostic::message::{get_text, Lang};
use tyml_generator::registry::STYLE_REGISTRY;
use tyml_source::SourceCode;

fn main() -> Result<(), String> {
    let args = std::env::args().collect::<Vec<_>>();

    if args.len() < 2 {
        return Err(get_text("binary.message.no_file_specified", Lang::system()));
    }

    let mut file = File::open(&args[1])
        .map_err(|_| get_text("binary.message.failed_to_read_file", Lang::system()))?;

    let mut source = String::new();

    file.read_to_string(&mut source)
        .map_err(|_| get_text("binary.message.failed_to_read_file", Lang::system()))?;

    let Some(header) = TymlHeader::parse(&source) else {
        return Err(get_text("binary.message.no_header_found", Lang::system()));
    };

    if let Some(Err(error)) = &header.style {
        return Err(
            get_text("binary.message.header_var_lookup_error", Lang::system())
                .replace("%0", &error.var_name),
        );
    }
    if let Err(error) = &header.tyml {
        match error {
            Either::Left(lookup_error) => {
                return Err(
                    get_text("binary.message.header_var_lookup_error", Lang::system())
                        .replace("%0", &lookup_error.var_name),
                );
            }
            Either::Right(_) => todo!(),
        }
    }

    let style = match &header.style {
        Some(style) => style.as_ref().unwrap().as_str(),
        None => args[1].split(".").last().unwrap(),
    };
    let tyml = header.tyml.unwrap();

    let Some(language) = STYLE_REGISTRY.resolve(style) else {
        return Err(
            get_text("binary.message.header_no_style_found", Lang::system()).replace("%0", style),
        );
    };

    let mut file = File::open(tyml.as_str()).map_err(|_| {
        get_text("binary.message.header_no_tyml_found", Lang::system()).replace("%0", tyml.as_str())
    })?;

    if !file
        .metadata()
        .map(|metadata| metadata.is_file())
        .unwrap_or(false)
    {
        return Err(
            get_text("binary.message.header_tyml_is_not_file", Lang::system())
                .replace("%0", tyml.as_str()),
        );
    }

    let mut tyml_source = String::new();

    file.read_to_string(&mut tyml_source).map_err(|_| {
        get_text("binary.message.failed_to_read_tyml", Lang::system()).replace("%0", tyml.as_str())
    })?;

    let tyml = TymlContext::new(SourceCode::new(tyml, tyml_source))
        .parse()
        .ml_parse_and_validate(
            &language,
            &SourceCode::new(
                Path::new(args[1].as_str())
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string(),
                source,
            ),
            None,
        );

    tyml.print_tyml_error(Lang::system());
    tyml.print_ml_parse_error(Lang::system());
    tyml.print_ml_validate_error(Lang::system());

    if tyml.has_tyml_error() || tyml.has_ml_parse_error() || tyml.has_ml_validate_error() {
        return Err(
            get_text("binary.message.has_error", Lang::system()).replace("%0", args[1].as_str())
        );
    }

    Ok(())
}
