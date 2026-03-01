# Dolang VS Code Extension

This extension provides syntax highlighting, bracket matching, and auto-closing features for "Dolang" (`.do` files) in Visual Studio Code.

## Folder Structure
Before installing, make sure your extension folder (`Dolang-vscode`) looks exactly like this:

```text
Dolang-vscode/
├── package.json
├── language-configuration.json
└── syntaxes/
    └── Dolang.tmLanguage.json
```

## Installation Instructions

To use this extension locally, you just need to copy the `Dolang-vscode` folder into your VS Code `extensions` directory. Choose your operating system below:

### macOS
1. Open your terminal.
2. Move or copy the `Dolang-vscode` folder into the VS Code extensions directory:
   ```bash
   cp -r /path/to/Dolang-vscode ~/.vscode/extensions/
   ```
3. Restart VS Code.

### Linux
1. Open your terminal.
2. Move or copy the `Dolang-vscode` folder into the VS Code extensions directory:
   ```bash
   cp -r /path/to/Dolang-vscode ~/.vscode/extensions/
   ```
3. Restart VS Code.
*(Note: If you are using the Flatpak version of VS Code, the path might be `~/.var/app/com.visualstudio.code/data/vscode/extensions/` instead).*

### Windows
1. Open File Explorer or Command Prompt.
2. Move or copy the `Dolang-vscode` folder into your user profile's VS Code extensions directory:
   ```cmd
   xcopy /E /I "C:\path\to\Dolang-vscode" "%USERPROFILE%\.vscode\extensions\Dolang-vscode"
   ```
   *Alternatively, you can just press `Win + R`, type `%USERPROFILE%\.vscode\extensions`, hit Enter, and drag-and-drop the folder there.*
3. Restart VS Code.

## Troubleshooting
* If the colors aren't showing up, ensure that the file extension is exactly `.do`.
* Check that the `package.json` file is directly inside the `Dolang-vscode` folder and not nested inside another folder.
* You can force VS Code to reload by pressing `Ctrl+Shift+P` (or `Cmd+Shift+P` on Mac) and running the **"Developer: Reload Window"** command.