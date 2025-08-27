use std::env::args;

use tokio::{fs::File, io::AsyncReadExt};
use tyml_core::{
    Tyml,
    tyml_diagnostic::{DiagnosticBuilder, message::Lang},
    tyml_source::SourceCode,
};
use tyml_mock::{
    TymlMock,
    error::{collect_send_error, collect_serve_error},
    server::ServerSourceLocation,
};

/// tyml-mock (serve | send) file-name [ interface-name ] [ function-name ]
#[tokio::main]
async fn main() -> Result<(), String> {
    let args: Vec<String> = args().collect();

    if args.len() < 3 {
        return Err("no argument".to_string());
    }

    let Ok(mut file) = File::open(args[2].as_str()).await else {
        return Err("failed to open file".to_string());
    };

    let mut string = String::new();

    file.read_to_string(&mut string).await.unwrap();

    let tyml = Tyml::parse(string);

    let tyml_source_code = SourceCode::new(args[2].clone(), tyml.source_code().clone());
    let empty_source_code = SourceCode::new(String::new(), String::new());

    if tyml.has_error() {
        for error in tyml.parse_errors().iter() {
            error.build(tyml.named_type_map()).print(
                Lang::system(),
                &tyml_source_code,
                &empty_source_code,
            );
        }

        for error in tyml.type_errors().iter() {
            error.build(tyml.named_type_map()).print(
                Lang::system(),
                &tyml_source_code,
                &empty_source_code,
            );
        }
    }

    let mock = TymlMock::new(tyml.clone());

    match args[1].as_str() {
        "serve" => {
            if args.len() == 3 {
                let mut errors = Vec::new();
                collect_serve_error(&tyml, ServerSourceLocation::AllInterfaces, &mut errors);

                if !errors.is_empty() {
                    for error in errors {
                        error.build(tyml.named_type_map()).print(
                            Lang::system(),
                            &tyml_source_code,
                            &empty_source_code,
                        );
                    }
                    return Err("error".to_string());
                }

                mock.server
                    .serve(ServerSourceLocation::AllInterfaces)
                    .await
                    .map_err(|_| "failed to serve".to_string())?;
            } else {
                let mut errors = Vec::new();
                collect_serve_error(&tyml, ServerSourceLocation::AllInterfaces, &mut errors);

                if !errors.is_empty() {
                    for error in errors {
                        error.build(tyml.named_type_map()).print(
                            Lang::system(),
                            &tyml_source_code,
                            &empty_source_code,
                        );
                    }
                    return Err("error".to_string());
                }

                let interface_name = args[3].as_str();
                mock.server
                    .serve(ServerSourceLocation::Interface(interface_name.to_string()))
                    .await
                    .map_err(|_| "failed to serve".to_string())?;
            }
        }
        "send" => {
            if args.len() < 5 {
                return Err("no interface and function name".to_string());
            }

            let interface_name = args[3].as_str();
            let function_name = args[4].as_str();

            let mut errors = Vec::new();
            collect_send_error(&tyml, interface_name, function_name, &mut errors);

            if !errors.is_empty() {
                for error in errors {
                    error.build(tyml.named_type_map()).print(
                        Lang::system(),
                        &tyml_source_code,
                        &empty_source_code,
                    );
                }
                return Err("error".to_string());
            }

            mock.client.send(interface_name, function_name).await?;
        }
        _ => {
            return Err(format!("invalid argument : {}", args[1].as_str()));
        }
    }

    Ok(())
}
