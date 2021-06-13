
# Table of Contents

1.  [Installation](#org240785e)
    1.  [Platforms supported](#org8629228)
    2.  [Emacs](#orgf0f1a8b)
        1.  [Arch-linux](#org7f4fb9e)
        2.  [Post installation](#orgbd51dd4)
2.  [Inspiration](#org4640be4)

The Nerdy Hamster Emacs, is my personal emacs configuration. That I am customizing after my needs.


<a id="org240785e"></a>

# Installation


<a id="org8629228"></a>

## Platforms supported

-   [X] Linux
-   [ ] Windows
-   [ ] MacOS

This Emacs configuration is currently only tested on `Arch Linux`.


<a id="orgf0f1a8b"></a>

## Emacs

> I am currently running Emacs `28.0.50` `Master` Branch, the repo is **only** tested on Version `28.0.50`.


<a id="org7f4fb9e"></a>

### Arch-linux

**Recomended** Install Emacs `28.0.50` or above.

    yay -S emacs-git


<a id="orgbd51dd4"></a>

### Post installation

Clone this repository to the `.emacs.d/` directory.

    git clone https://github.com/TheNerdyHamster/The-Nerdy-Hamster-Emacs.git ~/.emacs.d/

1.  Icons

    Packages such as `Centaur-tabs`, `Treemacs`, `doom-modeline` utilizes features from `all-the-icons`.
    
    When you start up emacs for the first time, run the following command.
    
        M-x (all-the-icons-install-font)


<a id="org4640be4"></a>

# Inspiration

[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) provides a good list of useful packages.

Other emacs/dotfiles repos for inspiration

-   [Daviwil's dotfiles](https://github.com/daviwil/dotfiles)
-   [Daviwil's emacs from scratch repo](https://github.com/daviwil/emacs-from-scratch)
-   [Angrybacon's dotemacs](https://github.com/angrybacon/dotemacs)

