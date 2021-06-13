
# Table of Contents

1.  [Installation](#orgfed0f94)
    1.  [Platforms supported](#orgd03aed8)
    2.  [Emacs](#org24d7cc4)
        1.  [Arch-linux](#orgbffb538)
        2.  [Post installation](#org2881d12)
2.  [Inspiration](#orgda00135)

The Nerdy Hamster Emacs, is my personal emacs configuration. That I am customizing after my needs.


<a id="orgfed0f94"></a>

# Installation


<a id="orgd03aed8"></a>

## Platforms supported

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Linux</td>
<td class="org-left">Yes</td>
</tr>


<tr>
<td class="org-left">Windows</td>
<td class="org-left">No</td>
</tr>


<tr>
<td class="org-left">MacOS</td>
<td class="org-left">No</td>
</tr>
</tbody>
</table>

This Emacs configuration is currently only tested on `Arch Linux`.


<a id="org24d7cc4"></a>

## Emacs

> I am currently running Emacs `28.0.50` `Master` Branch, the repo is **only** tested on Version `28.0.50`.


<a id="orgbffb538"></a>

### Arch-linux

**Recomended** Install Emacs `28.0.50` or above.

    yay -S emacs-git


<a id="org2881d12"></a>

### Post installation

Clone this repository to the `.emacs.d/` directory.

    git clone https://github.com/TheNerdyHamster/The-Nerdy-Hamster-Emacs.git ~/.emacs.d/

1.  Icons

    Packages such as `Centaur-tabs`, `Treemacs`, `doom-modeline` utilizes features from `all-the-icons`.
    
    When you start up emacs for the first time, run the following command.
    
        M-x (all-the-icons-install-font)


<a id="orgda00135"></a>

# Inspiration

[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs) provides a good list of useful packages.

Other emacs/dotfiles repos for inspiration

-   [Daviwil's dotfiles](https://github.com/daviwil/dotfiles)
-   [Daviwil's emacs from scratch repo](https://github.com/daviwil/emacs-from-scratch)
-   [Angrybacon's dotemacs](https://github.com/angrybacon/dotemacs)

