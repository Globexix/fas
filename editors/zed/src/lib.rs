use zed_extension_api::{self as zed, settings::LspSettings, LanguageServerId, Result};

struct FasExtension;

impl zed::Extension for FasExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary = LspSettings::for_worktree(language_server_id.as_ref(), worktree)?.binary;
        let mut args = vec!["lsp".to_string()];
        if let Some(extra) = binary.as_ref().and_then(|binary| binary.arguments.clone()) {
            args.extend(extra);
        }
        let command = match binary.and_then(|binary| binary.path) {
            Some(path) => path,
            None => worktree.which("fas").ok_or_else(|| {
                "fas not found on PATH; install fas or set lsp.fas.binary.path".to_string()
            })?,
        };
        Ok(zed::Command {
            command,
            args,
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(FasExtension);
