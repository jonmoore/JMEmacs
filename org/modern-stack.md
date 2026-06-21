# The Modern Completion and Action Stack

## Introduction

Emacs has historically offered various methods for completing user input, such as file names, buffer names, and commands. These often presented limitations, particularly with its horizontal display and a lack of immediate feedback during the typing process. Users often had to rely on repeated presses of the Tab key to reveal potential completions.

In response to these limitations, earlier "frameworks" like Helm and Ivy emerged. These frameworks introduced vertical displays for completion candidates, making navigation easier, and offered more sophisticated filtering mechanisms. Helm, for instance, is described as an incremental completion and selection narrowing framework that assists users in locating various items within Emacs. Similarly, Ivy aimed to be an efficient, small, simple, and smooth completion mechanism. However, they operated by replacing or heavily modifying Emacs's core completion mechanisms. This could lead to a steeper learning curve and introduce compatibility issues with other Emacs packages or conventions.

The modern stack, of packages including Vertico, Consult, and Embark is more modular and has a more standards-centric philosophy, enhancing rather than replacing the built-in completing-read function.

## Core Packages

### Vertico: The Vertical Completion User Interface

Vertico provides a streamlined vertical display for the completing-read function. Key features of Vertico include displaying the current candidate's index and the total number of candidates in the prompt. Vertico also incorporates efficient sorting algorithms, considering factors like history, length, and alphabetical order to improve the relevance of suggestions.

Vertico is intentionally minimalistic., concentrating on the user interface aspect of completion.

### Consult: Enhanced Navigation and Search

Consult provides commands built upon the completing-read function to enhance search and navigation within Emacs. The consult-buffer command offers an advanced interface for switching between buffers, recently opened files, and bookmarks, often with live previews. The consult-find command enhances file finding within projects, while consult-grep, consult-git-grep, and consult-ripgrep provide tools for searching file content using various backends. The consult-line command, allows users to search within the lines of the current buffer with live preview. Additionally, consult-outline facilitates navigating  the headings of a file.

Consult provides its commands with Vertico's completion interface. Many Consult commands also implement features like live previews and narrowing capabilities.

### Embark: Actions on Completion Candidates and Beyond

The third core component, Embark, introduces a unified framework for performing contextually relevant actions on various targets within Emacs, both during minibuffer completion sessions and in regular buffers. Embark's central command, embark-act, serves as a prefix key that brings up a keymap of actions relevant to the item at point or the currently selected completion candidate.

Embark also provides commands for working with sets of completion candidates. embark-collect takes the current list of candidates and presents them in a new buffer, allowing for perusal and further actions. embark-export opens the list of candidates in a buffer whose major mode is suited to the type of candidates. For instance, a list of files might be exported to a Dired buffer.

## Companion Packages

### Marginalia: Enriching Completions with Context

Marginalia adds annotations to the candidates displayed by Vertico and Consult. The annotations depend on the category of the completion candidates. For instance, when completing function names, Marginalia can display the associated key bindings and a brief description of the function.

### Orderless: Flexible and Intuitive Filtering

Traditionally, Emacs completion often relies on prefix or substring matching. Orderless allows users to type space-separated components of the candidate in any order.

Orderless supports various matching styles, including literal matching, regular expression matching, initialism matching, and flex matching (allowing for slight misspellings).

### Corfu: In-Buffer Completion for Enhanced Workflow

Corfu offers a minimalistic *in-buffer* completion UI, and displays a popup of completion candidates directly below or above the point in the current buffer. Corfu relies on Emacs's built-in completion-in-region functionality.

Corfu is compatible with Orderless.

### Wgrep: Editable Grep Buffers for Interactive Code Modification

Wgrep allows users to take the results of a grep search (e.g. from consult-grep, consult-git-grep, or consult-ripgrep) and open them in a special editable buffer.

Wgrep is often used with embark-export, with the export buffer automatically placed in Wgrep mode.

### Savehist and Recentf

When savehist-mode is enabled, Vertico uses its histories of commands, file names, and other inputs to place frequently-used completions higher in the list.

When recentf-mode is enabled, consult-buffer provides a convenient way to navigate to files that were recently worked on..

### Vertico Extensions

**vertico-multiform** allows users to configure different Vertico modes for specific commands or completion categories.

**vertico-grid** provides a grid-based display of completions.

