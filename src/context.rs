pub struct Context {
    jump_for_continue: String,
    jump_for_break: String,
}

impl Context {
    pub fn new(continue_label: String, break_label: String) -> Self {
        Context {
            jump_for_continue: continue_label,
            jump_for_break: break_label,
        }
    }

    pub fn continue_jump(&self) -> String {
        self.jump_for_continue.clone()
    }

    pub fn break_jump(&self) -> String {
        self.jump_for_break.clone()
    }
}
