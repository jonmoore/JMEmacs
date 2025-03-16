# **Enhancing Emacs Interaction: A Modern Completion and Action Stack**

## **Introduction: The Modern Emacs Completion and Action Landscape**

Emacs, renowned for its extensibility and customizability, has historically offered various methods for completing user input, such as file names, buffer names, and commands. Traditional Emacs completion, while functional, often presented limitations, particularly with its horizontal display and a lack of immediate feedback during the typing process 1. Users often had to rely on repeated presses of the Tab key to reveal potential completions, which could become cumbersome and less efficient, especially when dealing with a large number of options. This characteristic of older completion methods highlighted a need for more dynamic and user-friendly interfaces within Emacs.

In response to these limitations, earlier "frameworks" like Helm and Ivy emerged, providing significant advancements in the Emacs completion experience 2. These frameworks typically introduced vertical displays for completion candidates, making navigation easier, and offered more sophisticated filtering mechanisms. Helm, for instance, is described as an incremental completion and selection narrowing framework that assists users in locating various items within Emacs 11. Similarly, Ivy aimed to be an efficient, small, simple, and smooth completion mechanism 6. While these frameworks addressed many of the shortcomings of the default Emacs completion, they often operated by replacing or heavily modifying Emacs's core completion mechanisms. This comprehensive approach, while powerful, could sometimes lead to a steeper learning curve for users who had to familiarize themselves with the specific APIs and configurations of these frameworks 5. Furthermore, the extensive nature of these frameworks could occasionally introduce compatibility issues with other Emacs packages or deviate from standard Emacs behavior 5.

A more contemporary approach has gained prominence in the Emacs ecosystem with the rise of packages like Vertico, Consul, and Embark 2. This modern stack distinguishes itself by its modularity and a more Emacs-centric philosophy, focusing on enhancing rather than replacing the built-in completing-read function 2. By building upon this core Emacs functionality, these packages inherit its widespread compatibility and aim to provide a more integrated and predictable experience 2. The modular nature of this stack allows users to select and combine individual components according to their specific needs and preferences, leading to a highly customizable Emacs environment 2. This separation of concerns into distinct packages facilitates focused development and enables users to choose only the features they require, potentially resulting in a more lightweight and performant Emacs setup 2.

## **Core Components Revisited: Vertico, Consul, and Embark**

### **Vertico: The Vertical Completion User Interface**

At the heart of the modern Emacs completion stack lies Vertico, a package whose primary function is to provide a streamlined vertical display for the completing-read function 1. Unlike the traditional horizontal presentation of Emacs completions, Vertico presents candidates in a vertical list, which many users find more intuitive for browsing and selecting options using arrow keys 18. Key features of Vertico include displaying the current candidate's index and the total number of candidates in the prompt, offering clear feedback to the user about their position within the list 18. Furthermore, Vertico incorporates efficient sorting algorithms, considering factors like history, length, and alphabetical order to improve the relevance of suggestions 18.

Vertico is intentionally minimalistic, focusing on providing a user interface that behaves correctly and predictably across all Emacs completion scenarios 5. This design choice means that Vertico itself does not include many advanced features but instead relies on its compatibility with Emacs standards to ensure broad integration with other packages 18. By concentrating solely on the user interface aspect of completion, Vertico avoids feature bloat and can be seamlessly combined with other specialized packages to achieve enhanced functionality tailored to individual needs 18. The core strength of Vertico resides in its simplicity and its commitment to adhering to Emacs's underlying completion mechanisms, leading to a consistent and reliable user experience 5.

### **Consul: Enhanced Navigation and Search**

Complementing Vertico's user interface is Consul, a package that provides a suite of powerful commands built upon the completing-read function to enhance search and navigation within Emacs. Consul offers enhanced versions of many built-in Emacs commands, as well as entirely new commands designed to improve common workflows. Key commands provided by Consul include consult-buffer, which offers an advanced interface for switching between buffers, recently opened files, and bookmarks, often with live previews. The consult-find command enhances file finding within projects, while consult-grep, consult-git-grep, and consult-ripgrep provide asynchronous and interactive tools for searching file content using various backends. For in-buffer searching, Consul offers consult-line, allowing users to search within the lines of the current buffer with live preview. Additionally, consult-outline facilitates quick navigation through the headings of a file.

A significant aspect of Consul is its integration with Vertico, providing these enhanced search and navigation commands with Vertico's user-friendly vertical completion interface. Many Consul commands also implement features like live previews, which display the context of the currently selected candidate in another window, and narrowing capabilities, allowing users to further refine their search results by inputting additional criteria. Furthermore, Consul offers asynchronous search commands, enabling non-blocking operations for tasks like project-wide grepping, which is particularly beneficial for maintaining a responsive Emacs experience. In essence, Consul acts as the "brains" of this modern stack, providing powerful and versatile tools for interacting with Emacs data and external programs through the familiar and efficient completion paradigm.

### **Embark: Actions on Completion Candidates and Beyond**

The third core component, Embark, introduces a unified framework for performing contextually relevant actions on various targets within Emacs, both during minibuffer completion sessions and in regular buffers 1. Embark's central command, embark-act, serves as a prefix key that brings up a keymap of actions relevant to the item at point or the currently selected completion candidate 9. This functionality can be thought of as a keyboard-based equivalent of a right-click contextual menu, offering actions that are semantically appropriate for the identified target 9. For example, if the cursor is on a URL, embark-act might offer actions like opening the URL in a browser or downloading the associated file 9.

Embark also provides several powerful commands for working with sets of completion candidates. embark-collect takes the current list of candidates from the minibuffer and presents them in a new buffer, allowing for perusal and further actions 1. embark-export goes a step further by attempting to open the list of candidates in a buffer whose major mode is specifically suited to the type of candidates. For instance, a list of files might be exported to a Dired buffer, while a list of buffers could be exported to an Ibuffer buffer 1. Another useful command is embark-become, which allows users within the minibuffer to switch to a different command without losing the input they have already typed, offering a convenient way to correct a mistaken command invocation 9. Embark's integration with Vertico and Consul is crucial, as it provides the means to act upon the items selected or found using these packages, creating a more interactive and efficient Emacs experience 1.

## **Expanding the Ecosystem: Essential Companion Packages**

While Vertico, Consul, and Embark form the core of this modern Emacs interaction paradigm, several other packages commonly accompany them, further enhancing the user experience.

### **Marginalia: Enriching Completions with Context**

One such indispensable companion is Marginalia, a package that adds annotations to the completion candidates displayed by Vertico and Consul in the minibuffer 1. These annotations provide extra information about each candidate, offering immediate context without requiring the user to consult separate documentation or execute additional commands 47. The type of information displayed depends on the category of the completion candidates. For instance, when completing function names (as in M-x), Marginalia can display the associated key bindings and a brief description of the function 1. When completing file names, it might show file sizes and modification dates 1. For buffers, it could display the buffer's size and major mode 1.

Marginalia operates by identifying the completion category (e.g., command, file, buffer) and then using an appropriate annotator function to generate the annotations 47. Users can even cycle through different levels of detail or disable annotations altogether using the marginalia-cycle command 47. By providing this readily available contextual information, Marginalia significantly improves the usability of Vertico and Consul, reducing ambiguity and helping users to make more informed selections from completion lists 1.

### **Orderless: Flexible and Intuitive Filtering**

Another key package in this ecosystem is Orderless, which revolutionizes how users filter completion candidates 2. Traditionally, Emacs completion often relies on prefix or substring matching, requiring users to type the beginning of the desired candidate or a contiguous part of it. Orderless, however, allows users to type space-separated components of the candidate in any order, and it will match candidates that contain all of these components, regardless of their sequence 52. For example, when searching for the command package-install, a user could type "insta pack" and Orderless would still identify it as a match 52.

Orderless supports various matching styles, including literal matching, regular expression matching, initialism matching (matching by the first letters of words), and flex matching (allowing for slight misspellings) 53. This flexibility makes filtering completions more natural and efficient, as users can focus on the essential keywords they remember without being constrained by their order or exact spelling 2. Orderless seamlessly integrates with Vertico and Consul, enhancing their filtering capabilities and making it significantly easier to find the desired completion candidate, especially within long or complex lists 2.

### **Corfu: In-Buffer Completion for Enhanced Workflow**

For users who prefer completion within the main buffer rather than solely in the minibuffer, Corfu offers a minimalistic in-buffer completion UI 12. Unlike Vertico, which operates in the minibuffer, Corfu displays a popup of completion candidates directly below or above the point in the current buffer 57. Corfu relies on Emacs's built-in completion-in-region functionality, which is used by various modes for in-buffer completion 22. This allows Corfu to provide completion for identifiers, file paths, and other context-dependent items directly where the user is working 22.

Corfu is designed to be lightweight and efficient, focusing on providing a polished user interface for in-buffer completion 22. It supports navigation through the completion list using arrow keys and allows for candidate selection with the Tab or Return key 57. Notably, Corfu is also compatible with Orderless, allowing users to benefit from its flexible filtering capabilities even within the in-buffer completion context 22. Corfu serves as a valuable complement to Vertico, addressing the needs of users who prefer or require in-buffer completion for certain tasks, thus enhancing the overall Emacs workflow 12.

### **Wgrep: Editable Grep Buffers for Interactive Code Modification**

For tasks involving searching and replacing text across multiple files, Wgrep provides a powerful and interactive solution that integrates seamlessly with the modern Emacs stack 1. Wgrep allows users to take the results of a grep search (often initiated using Consul's consult-grep, consult-git-grep, or consult-ripgrep) and open them in a special editable buffer 1. In this buffer, users can directly edit the lines that matched the search query, and then apply these changes back to the original files 64.

Wgrep provides key bindings for managing these changes, such as C-c C-e to apply the edits to the file buffers and C-c C-u to discard changes 64. This interactive approach eliminates the need for complex sed scripts or manual editing of multiple files, making bulk search and replace operations significantly more efficient and user-friendly 1. Wgrep often works in conjunction with Embark, which can export the results of Consul's grep commands to an editable buffer that is automatically placed in Wgrep mode 1. This integration provides a streamlined workflow for finding specific text across a project and making necessary modifications directly from the search results.

### **Vertico Extensions: Tailoring the Minibuffer Experience**

Vertico's design emphasizes modularity, and it comes with several extensions that allow users to further customize its appearance and behavior 18. One notable extension is vertico-buffer, which enables Vertico to display the completion list in a separate, larger buffer instead of solely within the minibuffer 2. This can be particularly useful for users who prefer a more traditional interface reminiscent of Helm 2. Another popular extension is vertico-posframe, which utilizes the posframe package to show the Vertico completion list in a centered popup frame, providing a visually distinct and potentially less intrusive presentation 17.

Other extensions include vertico-multiform, which allows users to configure different Vertico modes for specific commands or completion categories, offering fine-grained control over the display 18. vertico-grid provides a grid-based display of completions, which can be beneficial for certain types of data 18. For users who prefer an even more minimal interface, vertico-unobtrusive displays only the topmost candidate 18. These extensions demonstrate Vertico's flexibility and allow users to adapt its core functionality to their individual preferences and specific use cases 18.

### **Savehist and Recentf: Enhancing Recall and Navigation**

While not strictly part of the Vertico/Consul/Embark ecosystem, the built-in Emacs features savehist-mode and recentf-mode significantly enhance the overall experience when using these packages 2. savehist-mode, when enabled, automatically saves the history of minibuffer inputs across Emacs sessions 2. This allows users to easily recall previously entered commands, file names, and other inputs using M-n (next-history-element) and M-p (previous-history-element) in the minibuffer 2. Vertico leverages this history to prioritize frequently used completions, placing them higher in the list, which further improves efficiency 2.

recentf-mode is another built-in feature that keeps track of recently opened files 35. This list of recently visited files can be readily accessed through Consul's consult-buffer command, providing a quick and convenient way to navigate back to files that were recently worked on 35. By remembering user history and recent activity, these built-in features complement the modern completion and action stack, making Emacs a more adaptive and user-friendly environment 2.

## **Synergistic Workflows: Combining Packages for Efficiency**

The true power of the modern Emacs stack becomes evident when these packages are used in combination to streamline common Emacs tasks.

For instance, when **finding a file**, a user might invoke consult-find (from Consul) to search within a project directory. To make the filtering more flexible, they can leverage Orderless, allowing them to type parts of the file name in any order 52. Vertico provides the efficient vertical display of the matching files 1. Once the file is selected, Embark could be used to perform actions on it, such as opening it in another window or copying its path to the clipboard 1.

Similarly, when **switching buffers**, consult-buffer (from Consul) offers an enhanced interface. Vertico provides the vertical list of buffers, and Marginalia can display useful information like the buffer's size and major mode 1. If a user decides they no longer need a particular buffer, they can use Embark to perform actions like killing the buffer directly from the consult-buffer list 1.

For **searching within a project**, consult-ripgrep (from Consul) is a powerful tool. Orderless can be used to filter the results based on keywords in any order 52. Vertico presents the search results in an organized manner 1. If the user needs to make changes to the matched lines, they can use embark-export to send the results to a buffer in grep mode, which can then be made editable with Wgrep, allowing for interactive modification of the code across multiple files 1.

When **executing commands** using M-x, Vertico provides the vertical completion list, and Marginalia displays helpful descriptions and key bindings for each command 1. Orderless enables users to find commands even if they only remember parts of the name or the words in a different order 52.

For **navigating within a file**, Consul offers commands like consult-outline and consult-imenu, which provide completion for headings and imenu items, respectively. Vertico presents these navigation points in a vertical list, and Marginalia can display additional information about each heading or item 1.

These examples illustrate how the modularity of the modern Emacs stack allows for the creation of highly efficient and customized workflows by combining the strengths of each individual package.

## **Conclusion: Embracing the Modern Emacs Stack**

The combination of Vertico, Consul, and Embark, along with their common companion packages like Marginalia, Orderless, Corfu, and Wgrep, represents a significant evolution in how users interact with Emacs. This modern stack offers numerous benefits, including improved efficiency through enhanced search and navigation, better discoverability of commands and options via annotations and flexible filtering, increased customizability allowing users to tailor their completion and action system to their specific needs, and a more integrated Emacs experience by building upon and extending the editor's core functionalities.

The modularity and flexibility of this approach empower users to construct a completion and action system that perfectly aligns with their individual workflows and preferences. For those looking to adopt or further enhance their Emacs setup, a recommended approach is to start with the core components of Vertico, Consul, and Marginalia, which provide a solid foundation for improved minibuffer interaction. Subsequently, users can gradually explore and add other packages like Orderless, Corfu, and Wgrep as their needs evolve. Furthermore, investigating Vertico extensions can unlock even more tailored behaviors and appearances.

In conclusion, the modern Emacs completion and action stack offers a compelling alternative to earlier frameworks, emphasizing modularity, integration with Emacs's core, and a focus on providing a highly customizable and efficient editing environment. By embracing these tools, users can significantly enhance their productivity and enjoy a more seamless and intuitive interaction with the powerful Emacs editor.

**Table 1: Comparison of Completion Frameworks**

| Feature | Helm | Ivy | Vertico \+ Companion Packages |
| :---- | :---- | :---- | :---- |
| UI Style | Typically separate window | Minibuffer or separate window | Primarily minibuffer (vertical list) |
| Core Principle | Comprehensive framework with many built-in features | Generic completion mechanism with focus on efficiency | Minimalistic UI built on completing-read, extended by modular packages |
| Extensibility | Highly extensible with many Helm-specific extensions | Extensible, with Counsel providing enhanced commands | Highly extensible through complementary packages like Marginalia, Orderless, Embark, and Vertico extensions |
| Reliance on completing-read | Has its own completion engine | Has its own completion engine | Directly uses and enhances completing-read |
| Actions Handling | Strong support for actions on completion candidates | Strong support for actions on completion candidates (via Counsel) | Actions handled by the separate Embark package, works on both minibuffer and buffer targets |
| Performance (general notes) | Can be resource-intensive | Generally fast and responsive | Lightweight core, performance depends on the specific combination of companion packages |

**Table 2: Key Packages in the Modern Emacs Stack**

| Package Name | Primary Function | Key Features | Integration with Vertico/Consul/Embark |
| :---- | :---- | :---- | :---- |
| Marginalia | Adds annotations to completion candidates | Displays extra information like key bindings, function descriptions, file sizes | Enhances Vertico and Consul by providing context in the minibuffer |
| Orderless | Flexible filtering of completion candidates | Matches space-separated components in any order, supports various matching styles | Integrates with Vertico and Consul for more powerful and intuitive filtering |
| Corfu | In-buffer completion UI | Displays completion popup near the point, arrow key navigation | Complements Vertico by providing completion within the main buffer, compatible with Orderless |
| Wgrep | Editable grep buffers | Allows editing grep results and applying changes to original files | Often used with Consul for initiating searches and Embark for exporting results |
| vertico-buffer | Vertico extension | Displays Vertico completions in a separate buffer | Extends Vertico's UI, offering a Helm-like experience |
| vertico-posframe | Vertico extension | Shows Vertico completions in a centered popup frame | Extends Vertico's UI, providing an alternative display method |
| Savehist | Built-in Emacs feature | Remembers minibuffer inputs across sessions | Vertico leverages this history to prioritize completions |
| Recentf | Built-in Emacs feature | Tracks recently opened files | Consul's consult-buffer can access the list of recent files |

**Table 3: Example Synergistic Workflows**

| Task | Packages Involved | Workflow Description |
| :---- | :---- | :---- |
| Finding a file | Consul, Orderless, Vertico, Embark | Use consult-find with Orderless for flexible filtering, Vertico for the UI, and Embark for actions on the found file. |
| Switching buffers | Consul, Vertico, Marginalia, Embark | Use consult-buffer with Vertico for the UI, Marginalia for buffer info, and Embark for actions like killing the buffer. |
| Searching within a project | Consul, Orderless, Vertico, Embark, Wgrep | Use consult-ripgrep with Orderless for filtering, Vertico for the UI, embark-export to a Wgrep-enabled buffer for editing. |
| Executing commands | Vertico, Marginalia, Orderless | Use M-x with Vertico for the UI, Marginalia for descriptions, and Orderless for fuzzy matching. |
| Navigating within a file | Consul, Vertico, Marginalia | Use consult-outline or consult-imenu with Vertico for the UI and Marginalia for heading/item information. |

#### **Works cited**

1\. Emacs: modern minibuffer packages (Vertico, Consult, etc.) \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=d3aaxOqwHhI](https://www.youtube.com/watch?v=d3aaxOqwHhI)  
2\. Streamline Your Emacs Completions with Vertico \- System Crafters, accessed March 15, 2025, [https://systemcrafters.net/emacs-tips/streamline-completions-with-vertico/](https://systemcrafters.net/emacs-tips/streamline-completions-with-vertico/)  
3\. Streamline Your Emacs Completions with Vertico \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=J0OaRy85MOo](https://www.youtube.com/watch?v=J0OaRy85MOo)  
4\. :completion vertico \- Doom Emacs v21.12 documentation, accessed March 15, 2025, [https://docs.doomemacs.org/v21.12/modules/completion/vertico/](https://docs.doomemacs.org/v21.12/modules/completion/vertico/)  
5\. From Ivy & Counsel to Vertico & Consult \- macOS & (open-source) Software, accessed March 15, 2025, [https://macowners.club/posts/from-ivy-to-vertico/](https://macowners.club/posts/from-ivy-to-vertico/)  
6\. abo-abo/swiper: Ivy \- a generic completion frontend for ... \- GitHub, accessed March 15, 2025, [https://github.com/abo-abo/swiper](https://github.com/abo-abo/swiper)  
7\. A Package in a league of its own: Helm, accessed March 15, 2025, [https://tuhdo.github.io/helm-intro.html](https://tuhdo.github.io/helm-intro.html)  
8\. Ivy User Manual \- or emacs, accessed March 15, 2025, [https://oremacs.com/swiper/](https://oremacs.com/swiper/)  
9\. System Crafters Live\! \- Replacing Ivy and Counsel with Vertico and Consult \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=UtqE-lR2HCA](https://www.youtube.com/watch?v=UtqE-lR2HCA)  
10\. Using Emacs Episode 80 \- Vertico, Marginalia, Consult, and Embark \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=5ffb2at2d7w](https://www.youtube.com/watch?v=5ffb2at2d7w)  
11\. Helm \- WikEmacs, accessed March 15, 2025, [https://wikemacs.org/wiki/Helm](https://wikemacs.org/wiki/Helm)  
12\. How many of you switched from ivy/counsel or helm to vertico/consult? : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/x7ahgz/how\_many\_of\_you\_switched\_from\_ivycounsel\_or\_helm/](https://www.reddit.com/r/emacs/comments/x7ahgz/how_many_of_you_switched_from_ivycounsel_or_helm/)  
13\. Helm | Emacs incremental completion and selection narrowing framework \- GitHub Pages, accessed March 15, 2025, [https://emacs-helm.github.io/helm/](https://emacs-helm.github.io/helm/)  
14\. Donning the Helm\! | Fuzzy Finding for Emacs | Switching to Emacs \#2 \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=ZzoqH2seOGY](https://www.youtube.com/watch?v=ZzoqH2seOGY)  
15\. emacs-helm/helm: Emacs incremental completion and ... \- GitHub, accessed March 15, 2025, [https://github.com/emacs-helm/helm](https://github.com/emacs-helm/helm)  
16\. Ivy User Manual \- Gnu ELPA, accessed March 15, 2025, [http://elpa.gnu.org/packages/doc/ivy.html](http://elpa.gnu.org/packages/doc/ivy.html)  
17\. doomemacs/modules/completion/vertico/README.org at master \- GitHub, accessed March 15, 2025, [https://github.com/doomemacs/doomemacs/blob/master/modules/completion/vertico/README.org](https://github.com/doomemacs/doomemacs/blob/master/modules/completion/vertico/README.org)  
18\. emacs-straight/vertico: Mirror of the vertico package from GNU ELPA, current as of 2025-02-04 \- GitHub, accessed March 15, 2025, [https://github.com/emacs-straight/vertico](https://github.com/emacs-straight/vertico)  
19\. What are the benefits of Vertico over Helm or Ivy? : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/117zdnu/what\_are\_the\_benefits\_of\_vertico\_over\_helm\_or\_ivy/](https://www.reddit.com/r/emacs/comments/117zdnu/what_are_the_benefits_of_vertico_over_helm_or_ivy/)  
20\. GNU-devel ELPA \- consult, accessed March 15, 2025, [https://elpa.gnu.org/devel/consult.html](https://elpa.gnu.org/devel/consult.html)  
21\. Have the cool kids moved from Ivy & Counsel to Vertico & Consult? : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/v9w03w/have\_the\_cool\_kids\_moved\_from\_ivy\_counsel\_to/](https://www.reddit.com/r/emacs/comments/v9w03w/have_the_cool_kids_moved_from_ivy_counsel_to/)  
22\. minad/vertico: :dizzy: vertico.el \- VERTical Interactive COmpletion \- GitHub, accessed March 15, 2025, [https://github.com/minad/vertico](https://github.com/minad/vertico)  
23\. radian-software/selectrum: Better solution for incremental narrowing in Emacs. \- GitHub, accessed March 15, 2025, [https://github.com/radian-software/selectrum](https://github.com/radian-software/selectrum)  
24\. Improving my Emacs experience with completion \- Hacker News, accessed March 15, 2025, [https://news.ycombinator.com/item?id=39187370](https://news.ycombinator.com/item?id=39187370)  
25\. vertico \- GNU-devel ELPA, accessed March 15, 2025, [https://elpa.gnu.org/devel/vertico.html](https://elpa.gnu.org/devel/vertico.html)  
26\. System Crafters Live\! \- Trying New Emacs Packages: Vertico and Corfu \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=nyz4O7EwxIk](https://www.youtube.com/watch?v=nyz4O7EwxIk)  
27\. My understanding of various sorts of completion in GNU Emacs, accessed March 15, 2025, [https://utcc.utoronto.ca/\~cks/space/blog/programming/EmacsUnderstandingCompletion](https://utcc.utoronto.ca/~cks/space/blog/programming/EmacsUnderstandingCompletion)  
28\. oantolin/embark: Emacs Mini-Buffer Actions Rooted in Keymaps \- GitHub, accessed March 15, 2025, [https://github.com/oantolin/embark](https://github.com/oantolin/embark)  
29\. Helm vs Ivy: What are the differences, what are the advantages? : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/7vcrwo/helm\_vs\_ivy\_what\_are\_the\_differences\_what\_are\_the/](https://www.reddit.com/r/emacs/comments/7vcrwo/helm_vs_ivy_what_are_the_differences_what_are_the/)  
30\. Emacs from Source Part 2: vertico, orderless, and marginalia \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=4911q\_SnGDU](https://www.youtube.com/watch?v=4911q_SnGDU)  
31\. Clearly explain Emacs completion ecosystem in README · Issue \#274 · radian-software/selectrum \- GitHub, accessed March 15, 2025, [https://github.com/raxod502/selectrum/issues/274](https://github.com/raxod502/selectrum/issues/274)  
32\. From helm, to ivy · Samuel Barreto \- Bacterial Finches, accessed March 15, 2025, [https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/](https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/)  
33\. Ivy To Selectrum \- Irreal.org, accessed March 15, 2025, [https://irreal.org/blog/?p=9699](https://irreal.org/blog/?p=9699)  
34\. embark-consult \- GNU ELPA, accessed March 15, 2025, [https://elpa.gnu.org/packages/embark-consult.html](https://elpa.gnu.org/packages/embark-consult.html)  
35\. Emacs: modern minibuffer packages (Vertico, Consult, etc.) | Protesilaos Stavrou, accessed March 15, 2025, [https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/](https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/)  
36\. How save changed in all buffers (Embark export: consult-grep)? \- Emacs Stack Exchange, accessed March 15, 2025, [https://emacs.stackexchange.com/questions/80486/how-save-changed-in-all-buffers-embark-export-consult-grep](https://emacs.stackexchange.com/questions/80486/how-save-changed-in-all-buffers-embark-export-consult-grep)  
37\. Posframe Multi-head Awareness in EXWM \- Emacs \- System Crafters Forum, accessed March 15, 2025, [https://forum.systemcrafters.net/t/posframe-multi-head-awareness-in-exwm/515](https://forum.systemcrafters.net/t/posframe-multi-head-awareness-in-exwm/515)  
38\. \[GNU ELPA\] Vertico-Posframe version 0.7.7, accessed March 15, 2025, [https://mail.gnu.org/archive/html/gnu-emacs-sources/2024-02/msg00003.html](https://mail.gnu.org/archive/html/gnu-emacs-sources/2024-02/msg00003.html)  
39\. vertico/extensions/vertico-buffer.el at main · minad/vertico \- GitHub, accessed March 15, 2025, [https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el](https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el)  
40\. How to Use Embark for Emacs \- Alex's notes, accessed March 15, 2025, [https://notes.alexkehayias.com/how-to-use-embark-for-emacs/](https://notes.alexkehayias.com/how-to-use-embark-for-emacs/)  
41\. Emacs: Embark and my extras \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=uoP9ZYdNCHg](https://www.youtube.com/watch?v=uoP9ZYdNCHg)  
42\. Embark: Emacs Mini-Buffer Actions Rooted in Keymaps \- Gnu ELPA, accessed March 15, 2025, [https://elpa.gnu.org/devel/doc/embark.html](https://elpa.gnu.org/devel/doc/embark.html)  
43\. Emacs packages ednc, vertico, consult, embark, marginalia, orderless and fd-dired, accessed March 15, 2025, [https://www.youtube.com/watch?v=gal3YxBkusY](https://www.youtube.com/watch?v=gal3YxBkusY)  
44\. System Crafters Live\! \- The Many Uses of Embark \- YouTube, accessed March 15, 2025, [https://www.youtube.com/watch?v=qk2Is\_sC8Lk](https://www.youtube.com/watch?v=qk2Is_sC8Lk)  
45\. Warp Factor Refactoring in Emacs | Lambda Land \- Ashton Wiersdorf, accessed March 15, 2025, [https://lambdaland.org/posts/2023-05-31\_warp\_factor\_refactor/](https://lambdaland.org/posts/2023-05-31_warp_factor_refactor/)  
46\. Those of you who use Vertico/Consult, what are top most useful for you commands / features? : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/1abpb4q/those\_of\_you\_who\_use\_verticoconsult\_what\_are\_top/](https://www.reddit.com/r/emacs/comments/1abpb4q/those_of_you_who_use_verticoconsult_what_are_top/)  
47\. minad/marginalia: :scroll: marginalia.el \- Marginalia in the minibuffer \- GitHub, accessed March 15, 2025, [https://github.com/minad/marginalia](https://github.com/minad/marginalia)  
48\. Debian \-- Details of package elpa-marginalia in bookworm, accessed March 15, 2025, [https://packages.debian.org/bookworm/elpa-marginalia](https://packages.debian.org/bookworm/elpa-marginalia)  
49\. marginalia \- GNU-devel ELPA, accessed March 15, 2025, [https://elpa.gnu.org/devel/marginalia.html](https://elpa.gnu.org/devel/marginalia.html)  
50\. emacscollective/epkg-marginalia: Show Epkg information in completion annotations \- GitHub, accessed March 15, 2025, [https://github.com/emacscollective/epkg-marginalia](https://github.com/emacscollective/epkg-marginalia)  
51\. Emacs Commands Explanation Via Marginalia Annotation 2024\_08\_12\_04:10:20, accessed March 15, 2025, [https://www.youtube.com/watch?v=M1DqNxMp8ws](https://www.youtube.com/watch?v=M1DqNxMp8ws)  
52\. Understanding the orderless package for GNU Emacs, accessed March 15, 2025, [https://utcc.utoronto.ca/\~cks/space/blog/programming/EmacsUnderstandingOrderless](https://utcc.utoronto.ca/~cks/space/blog/programming/EmacsUnderstandingOrderless)  
53\. oantolin/orderless: Emacs completion style that matches multiple regexps in any order \- GitHub, accessed March 15, 2025, [https://github.com/oantolin/orderless](https://github.com/oantolin/orderless)  
54\. orderless \- GNU-devel ELPA, accessed March 15, 2025, [https://elpa.gnu.org/devel/orderless.html](https://elpa.gnu.org/devel/orderless.html)  
55\. orderless/orderless.el at master · oantolin/orderless \- GitHub, accessed March 15, 2025, [https://github.com/oantolin/orderless/blob/master/orderless.el](https://github.com/oantolin/orderless/blob/master/orderless.el)  
56\. Help with completion-styles and orderless : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/zz07eg/help\_with\_completionstyles\_and\_orderless/](https://www.reddit.com/r/emacs/comments/zz07eg/help_with_completionstyles_and_orderless/)  
57\. minad/corfu: :desert\_island: corfu.el \- COmpletion in Region FUnction \- GitHub, accessed March 15, 2025, [https://github.com/minad/corfu](https://github.com/minad/corfu)  
58\. emacs-corfu package : Ubuntu \- Launchpad, accessed March 15, 2025, [https://launchpad.net/ubuntu/+source/emacs-corfu](https://launchpad.net/ubuntu/+source/emacs-corfu)  
59\. emacs-corfu-terminal \- Debian Package Tracker, accessed March 15, 2025, [https://tracker.debian.org/pkg/emacs-corfu-terminal](https://tracker.debian.org/pkg/emacs-corfu-terminal)  
60\. corfu \- GNU-devel ELPA, accessed March 15, 2025, [https://elpa.gnu.org/devel/corfu.html](https://elpa.gnu.org/devel/corfu.html)  
61\. NonGNU ELPA \- corfu-terminal, accessed March 15, 2025, [https://elpa.nongnu.org/nongnu/corfu-terminal.html](https://elpa.nongnu.org/nongnu/corfu-terminal.html)  
62\. Company, vertico(+), corfu \= corfusion :-) : r/emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/1bz0ekn/company\_vertico\_corfu\_corfusion/](https://www.reddit.com/r/emacs/comments/1bz0ekn/company_vertico_corfu_corfusion/)  
63\. app-emacs/wgrep \- Gentoo Packages, accessed March 15, 2025, [https://packages.gentoo.org/packages/app-emacs/wgrep](https://packages.gentoo.org/packages/app-emacs/wgrep)  
64\. mhayashi1120/Emacs-wgrep: Writable grep buffer and apply the changes to files \- GitHub, accessed March 15, 2025, [https://github.com/mhayashi1120/Emacs-wgrep](https://github.com/mhayashi1120/Emacs-wgrep)  
65\. NonGNU-devel ELPA \- wgrep, accessed March 15, 2025, [https://elpa.nongnu.org/nongnu-devel/wgrep.html](https://elpa.nongnu.org/nongnu-devel/wgrep.html)  
66\. emacs-wgrep package : Ubuntu \- Launchpad, accessed March 15, 2025, [https://launchpad.net/ubuntu/+source/emacs-wgrep](https://launchpad.net/ubuntu/+source/emacs-wgrep)  
67\. Emacs-wgrep/wgrep-ag.el at master · mhayashi1120/Emacs-wgrep \- GitHub, accessed March 15, 2025, [https://github.com/mhayashi1120/Emacs-wgrep/blob/master/wgrep-ag.el](https://github.com/mhayashi1120/Emacs-wgrep/blob/master/wgrep-ag.el)  
68\. vertico-posframe \- GNU ELPA, accessed March 15, 2025, [https://elpa.gnu.org/packages/vertico-posframe.html](https://elpa.gnu.org/packages/vertico-posframe.html)  
69\. tumashu/vertico-posframe \- GitHub, accessed March 15, 2025, [https://github.com/tumashu/vertico-posframe](https://github.com/tumashu/vertico-posframe)  
70\. vertico-posframe.el \- GitHub, accessed March 15, 2025, [https://github.com/tumashu/vertico-posframe/blob/main/vertico-posframe.el](https://github.com/tumashu/vertico-posframe/blob/main/vertico-posframe.el)  
71\. emacs-release/lisp/savehist.el at master \- GitHub, accessed March 15, 2025, [https://github.com/jwiegley/emacs-release/blob/master/lisp/savehist.el](https://github.com/jwiegley/emacs-release/blob/master/lisp/savehist.el)  
72\. emacs/lisp/savehist.el at master \- GitHub, accessed March 15, 2025, [https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el)  
73\. The 6 Emacs Settings Every User Should Consider \- System Crafters, accessed March 15, 2025, [https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/](https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/)  
74\. Savehist-mode \- emacs \- Reddit, accessed March 15, 2025, [https://www.reddit.com/r/emacs/comments/11w2d1e/savehistmode/](https://www.reddit.com/r/emacs/comments/11w2d1e/savehistmode/)
