use std::process::Stdio;

#[non_exhaustive]
pub struct SuperCtx {}

impl SuperCtx {
    pub fn new() -> Self {
        Self {}
    }

    pub fn spawn(&mut self, cmd: &std::process::Command) -> std::io::Result<std::process::Child> {
        if !matches!(cmd.get_program().to_str().unwrap(), "flatpak" | "zypper") {
            panic!("Maybe we shouldn't be running sudo on this");
        }

        std::process::Command::new("sudo")
            .arg(cmd.get_program())
            .args(cmd.get_args())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
    }
}
