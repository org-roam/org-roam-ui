# org-roam-ui

org-roam-ui is a successor of https://github.com/org-roam/org-roam-server that extends functionality of org-roam with a Web app that runs side-by-side with Emacs.

## Disclaimers

- We only support [org-roam v2](https://blog.jethro.dev/posts/org_roam_v2/); v1 will never be supported.
- Feature-parity with org-roam-server is not the goal althought we aim to make a similar product which is having a visial graph to help you explore and navigate your org-roam nodes.
- This is alpha software: please do give it a try and use it, but expect bugs and troubleshooting.
- The project was created by a couple of tinkerers to scratch their own itch. We don't get rewarded in any material way and may stop any day (because life). The best way to keep the project alive is to [explore the code and contribute](#Contribute).

## Install

org-roam-ui is not on MELPA.

Clone the repo:
```bash
cd ~/.emacs.d/private
git clone git@github.com:org-roam/org-roam-ui.git
```

Load in Emacs (add to config):
```lisp
(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
(load-library "org-roam-ui")
```

Should your Emacs miss any of the dependecies, please install them manually.

## Use

Use `M-x org-roam-ui RET` to enable the global mode. 
It will start a web server on http://127.0.0.1:35901/.

## Org-Roam Protocol

The graph utilizes org-roam protocol which means if you click on one
of the nodes, it will open the corresponding file in Emacs. For this
feature to work, org-roam protocol should be configured in the system.

[Configuring Org-Roam Protocol](https://www.orgroam.com/manual.html#Installation-_00281_0029)

Also make sure the emacs server is started; `M-x server-start RET`

# Contribute

[GitHub Community Guidelines
](https://docs.github.com/en/github/site-policy/github-community-guidelines) apply.

Go [here](https://github.com/org-roam/org-roam-ui/discussions/4) to have an onboarding call with a member of the core team.

We would ❤️ to have you on board.
