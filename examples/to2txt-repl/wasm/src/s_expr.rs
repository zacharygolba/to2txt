use std::fmt::{self, Display, Formatter};
use to2txt::{Tag, Task};

pub struct WriteSExpr<'a> {
    task: &'a Task<'a>,
}

impl<'a> WriteSExpr<'a> {
    pub fn new(task: &'a Task) -> Self {
        Self { task }
    }
}

impl Display for WriteSExpr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "(line {}", self.task.line())?;
        writeln!(f, "{:>2}(task", " ")?;

        if let Some(x) = &self.task.x {
            writeln!(f, "{:>4}(x {:?})", " ", x.offset())?;
        }

        if let Some(priority) = &self.task.priority {
            writeln!(
                f,
                "{:>4}(priority {:?} {})",
                " ",
                priority.offset(),
                priority.value(),
            )?;
        }

        if let Some(finished_on) = &self.task.finished_on {
            writeln!(
                f,
                "{:>4}(finished_on {:?} {})",
                " ",
                finished_on.offset(),
                finished_on.value(),
            )?;
        }

        if let Some(started_on) = &self.task.started_on {
            writeln!(
                f,
                "{:>4}(started_on {:?} {})",
                " ",
                started_on.offset(),
                started_on.value(),
            )?;
        }

        let description = &self.task.description;
        let n_tags = self.task.tags().count();

        write!(
            f,
            "{:>4}(description {:?} {:?}{}",
            " ",
            description.offset(),
            description.fragment(),
            if n_tags > 0 { "" } else { ")))" },
        )?;

        for (index, tag) in self.task.tags().enumerate() {
            if index == 0 {
                writeln!(f, "")?;
            }

            match tag.value() {
                Tag::Context(context) => {
                    let offset = context.offset();
                    let value = context.value();

                    write!(f, "{:>6}(context {:?} {:?})", " ", offset, value)?;
                }
                Tag::Project(project) => {
                    let offset = project.offset();
                    let value = project.value();

                    write!(f, "{:>6}(project {:?} {:?})", " ", offset, value)?;
                }
                Tag::Named(key, value) => {
                    let offset = (key.start(), value.end());
                    let value = (key.fragment(), value.fragment());

                    write!(f, "{:>6}(named {:?} {:?})", " ", offset, value)?;
                }
            };

            writeln!(f, "{}", if index == n_tags - 1 { ")))" } else { "" })?;
        }

        Ok(())
    }
}
