# Nederlands-Engels

A small program to match English sentences with their Dutch translations.

![Nederlands-Engels screenshot](./Docs/img/screenshot.png)

## Usage

Only keyboard navigation is supported.

Hotkeys:

- `←` to move to the English sentence
- `→` to move to the Dutch sentence
- `↑` to move to the previous sentence
- `↓` to move to the next sentence
- `m` to merge the currently selected sentence with the previous one
- `u`/`r` to undo/redo last changes
- `Esc`/`q` to save the current state and quit

Repository also contains three chapters of the "A Study in Scarlet" book by Arthur Conan Doyle and their translations to Dutch.

## Building

```shell
# Clone the repository and its submodules
git clone --recurse-submodules git@github.com:Thecentury/nederlands-engels.git
cd nederlands-engels/NederlandsEngels.Gui
# Build the application
dotnet build
# Run it
dotnet run
```