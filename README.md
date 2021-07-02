
# Table of Contents

1.  [Installation](#org683805a)
    1.  [Platforms supported](#org11f6025)
    2.  [Emacs](#orgd190dae)
        1.  [Arch-linux](#org8836bb6)
        2.  [Post installation](#org2cfe0ee)
2.  [Inspiration](#orgf26f504)

The Nerdy Hamster Emacs, is my personal emacs configuration. That I am customizing after my needs.


<a id="org683805a"></a>

# Installation


<a id="org11f6025"></a>

## Platforms supported

-   [X] Linux
-   [ ] Windows
-   [ ] MacOS

This Emacs configuration is currently only tested on `Arch Linux`.


<a id="orgd190dae"></a>

## Emacs

> I am currently running Emacs `28.0.50` `Master` Branch, the repo is **only** tested on Version `28.0.50`.


<a id="org8836bb6"></a>

### Arch-linux

**Recomended** Install Emacs `28.0.50` or above.

    paru -S emacs-git nerd-fonts-fira-code


<a id="org2cfe0ee"></a>

### Post installation

Clone this repository to the `.emacs.d/` directory.

    git clone --recursive git@git.sr.ht:~thenerdyhamster/tnh-emacs ~/.emacs.d 

1.  Secrets

    Create a file called `.secrets.el` that contains
    
        (setq tnh/wk-token "value")
        (setq tnh/vpn-host "value")
        (setq tnh/vpn-user "value")
        (setq tnh/vpn-cert "value")

2.  Icons

    Packages such as `Centaur-tabs`, `Treemacs`, `doom-modeline` utilizes features from `all-the-icons`.
    
    When you start up emacs for the first time, run the following command.
    
        M-x (all-the-icons-install-font)


<a id="orgf26f504"></a>

# Inspiration

[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) provides a good list of useful packages.

Other emacs/dotfiles repos for inspiration

-   [Daviwil's dotfiles](https://github.com/daviwil/dotfiles)
-   [Daviwil's emacs from scratch repo](https://github.com/daviwil/emacs-from-scratch)
-   [Angrybacon's dotemacs](https://github.com/angrybacon/dotemacs)

