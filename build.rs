use std::fs::File;
use std::io::{Write, Read, Result};
use std::path::Path;

fn main() -> Result<()> {
    generate_readme_md()?;
    Ok(())
}

/// Generate the README.md documentation file at build time.
fn generate_readme_md() -> Result<()> {
    let docs_path = "doc";
    let readme_path = "README.md";

    // List of doc files in the order we want them to appear in the README.md file
    let doc_filenames = vec![
        "tracing_config_purpose.md",
        "performance_penalties.md",
        "getting_started.md",
        "configuration_file_search_path.md",
        "configuration_file.md",
        "basic_configuration_file.md",
        "reference_links.md",
    ];

    let mut readme = String::new();

    // Iterate over the files and append their contents to `new_contents`
    for doc_filename in doc_filenames {
        let mut doc_file = File::open(format!("{}/{}", docs_path, doc_filename))?;
        let mut doc_contents = String::new();
        doc_file.read_to_string(&mut doc_contents)?;
        readme.push_str(&doc_contents);
        readme.push('\n'); // add two newline characters between files to separate sections / paragraphs
        readme.push('\n');
    }

    // pop off the last 2 new lines
    readme.pop();
    readme.pop();

    // Read the current README.md, if it exists
    let existing_readme = if Path::new(readme_path).exists() {
        let mut readme_file = File::open(readme_path)?;
        let mut existing_readme = String::new();
        readme_file.read_to_string(&mut existing_readme)?;
        existing_readme
    } else {
        String::new()
    };

    // Write to README.md only if the contents are different
    if readme != existing_readme {
        let mut readme_file = File::create(readme_path)?;
        readme_file.write_all(readme.as_bytes())?;
    }

    Ok(())
}
