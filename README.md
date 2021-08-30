# org-roam-ui: an org-roam frontend

[![ci](https://github.com/org-roam/org-roam-ui/actions/workflows/ci.yml/badge.svg)](https://github.com/org-roam/org-roam-ui/actions/workflows/ci.yml)

![image](https://user-images.githubusercontent.com/21983833/127746882-4ba00691-3be4-49d6-8c8c-e139a14596c2.png)

Org-Roam-UI is a frontend for exploring and interacting with your [org-roam](https://github.com/org-roam/org-roam) notes.

Org-Roam-UI is meant a successor of [org-roam-server](https://github.com/org-roam/org-roam-server) that extends functionality of org-roam with a Web app that runs side-by-side with Emacs.

## Changelog

For major new features/bugfixes we will update [changelog](https://github.com/org-roam/org-roam-ui/discussions/30).

## Installation

`org-roam-ui` is not (yet!) on MELPA.

Should your Emacs miss any of the dependencies, please install them manually!
org-roam-ui requires `org-roam`, `websocket`, `simple-httpd`, `f` and Emacs >= 27 for fast JSON parsing.

### Manually

Install `websocket.el` and `simple-httpd`

```emacs-lisp
M-x package-install websocket
M-x package-install simple-httpd
```

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

### Doom

Add the following to your `package.el`

Org-roam-ui tries to keep up with the latest features of `org-roam`, which conflicts with Doom Emacs's desire for
stability. To make sure nothing breaks, use the latest version of `org-roam` by unpinning it.
```emacs-lisp
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
```

Then something along the following to your `config.el`

```emacs-lisp
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

```

We recommend only loading org-roam-ui after loading org(-roam) as starting the server and making database requests can impact startup times quite a lot.

### straight/use-package

```emacs-lisp
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

```

## Usage

Use `M-x org-roam-ui-mode RET` to enable the global mode.
It will start a web server on http://127.0.0.1:35901/ and connect to it via a WebSocket for real-time updates.

### Commands

ORUI provides a few commands for interacting with the graph without ever having to leave Emacs.
NOTE: This is quite janky at the moment and will change in the future. Consider this more of a teaser.

#### Moving around

```emacs-lisp
(orui-node-zoom)
```

Zooms to the current node in the global view _ignoring local mode_.

```emacs-lisp
(orui-node-local)
```

Opens the current node in local view.

You can optionally give these command three parameters:

1. the node id you want to zoom to (by default the current node)
2. The speed at which you want to zoom (can be set in the UI) in ms.
3. The padding of the zoom in px.

These options might not work at the moment, please configure them in the UI for the time being.

### Configuration

Org-Roam-UI exposes a few variables, but most of the customization is done in the web app.

#### Following

ORUI follows you around Emacs by default. To disable this, set

```emacs-lisp
(setq org-roam-ui-follow nil)
```

or disable the minor mode `org-roam-ui-follow-mode`.

#### Updating

We plan to make updates to the graph happen smoothly, at the moment it is only possible to reload the entire graph when an update happens (but local mode is preserved). This is enabled by default, to disable

```emacs-lisp
(setq org-roam-ui-update-on-save nil)
```

#### Theme

Org-Roam-UI can sync your Emacs theme! This is the default behavior, to disable it do

```emacs-lisp
(setq org-roam-ui-sync-theme nil)
```

Then call `M-x orui-sync-theme`.

You can also provide your own theme if you do not like syncing nor like the default one. To do so, set `org-roam-ui-custom-theme` to an alist of (rather specific) variables, like so

```emacs-lisp
(setq org-roam-ui-custom-theme
    '((bg . "#1E2029")
        (bg-alt . "#282a36")
        (fg . "#f8f8f2")
        (fg-alt . "#6272a4")
        (red . "#ff5555")
        (orange . "#f1fa8c")
        (yellow ."#ffb86c")
        (green . "#50fa7b")
        (cyan . "#8be9fd")
        (blue . "#ff79c6")
        (violet . "#8be9fd")
        (magenta . "#bd93f9")))
```

You can optionally provide `(base1 . "#XXXXXX")` arguments after the last one to also set the background shades, otherwise ORUI will guess based on the provides bg and fg.

### Open on start

By default, org-roam-ui will try to open itself in your default browser. To disable this, set

```emacs-lisp
(setq org-roam-ui-open-on-start nil)
```

## Disclaimers ‼

- We only support [org-roam v2](https://blog.jethro.dev/posts/org_roam_v2/); v1 will never be supported.
- As the name suggests, Org-Roam-UI only works with org-roam! If you organize your notes in some other form org-roam-ui cannot work, as it uses org-roam to fetch all the connections.
- Feature-parity with org-roam-server is not the goal. Although we aim to make a similar product which is having a visual graph to help you explore and navigate your org-roam nodes, we do not intend to replicate all of the, nor be limited to replicating the features of org-roam-server.
- This is alpha software: please do give it a try and use it, but expect bugs and troubleshooting!
- The project was created by a couple of tinkerers to scratch their own itch. We don't get rewarded in any material way and development may stop any day (because life). The best way to keep the project alive is to [explore the code and contribute](#Contribute)!

## FAQ 🗨

## Q: Aaaaand it broke: what do?

Sorry! This is still alpha software, so expect it to break from time to time. Best thing you can try is to remove your settings by going to "Storage > Local Storage" on Firefox or "Application > Local Storage" on Chromium and deleting everything there.

If the issue still persists, please file a bug report with

1. Your browsers console log
2. Your browsers
3. What you were doing when it broke

and we'll try to help you ASAP!

## Q: Clicking 'Open in Emacs' gives an error around json-parse-string, how do I fix this?

If you receive an error, in emacs, stating `function definition is void json-parse-string`, then you must compile emacs with json support.  This is not automatically done on systems such as Gentoo.

### Q: Graph Slow! Faster?

While we try to optimize the display of the graph, there is only so much we can do. For largish networks (>2k nodes) dragging the graph around a lot can cause some performance issues, but there are a few things you can do to speed it up.

#### Close the tweaks panel

At the time of writing (Aug 8) it is very much not optimized, and shifting between global and local mode or 2d or 3d is noticeably slower with the tweaks panel open than without. This will be fixed in a future release.

#### Use a Chromium based browser

As much as it saddens us to say, Firefox's rendering engine is quite a bit slower than its Chromium cousins. Compare the performance of the two and see if that's the main issue first.

#### Turn of the particles

I know, very cool to see those little guys travel up and down your notes, but very slow, especially in 3D mode.

#### Turn off labels

Probably the second slowest thing to render, with little possibility of speeding it up. Consider only turning on labels on highlight or cranking up the "Label appearance scale".

#### Turn off highlight animations

I know, they're gorgeous, but not very performant.

#### Turn off collision

Nice, but costly! If you like to have the graph more spread out, turning off collision will change little in the resulting layout, but will help performance quite a bit.

#### Turn off gravity

Fewer forces fewer worries

#### Favor 2D over 3D

I know, it looks cool, but man is it slow.

#### Don't drag the dang thing around so much!

In our experience, once the graph has actually settled and nothing needs to be rendered again, looking around should pose little trouble. At the moment there is no way of "saving" the graph configuration, but we are exploring the possibility. The graph layout algorithm is deterministic however, so barring any changes to the data it should produce the same results each time.

### Q: Will you implement X?

Hopefully, yeah! But time is limited, and so is the amount of features we can cram into this things before it implodes in itself, so we are adding things incrementally to make sure they work. That said, we'd love to hear from you! 
If your feature is not already on the [project board](https://github.com/org-roam/org-roam-ui/projects/2), please post minor feature requests such as "I want to be able to color this specific node" in [the minor feature requests discussion](https://github.com/org-roam/org-roam-ui/discussions/6) and *major* feature requests (e.g. "I want to publish my graph) in [the major feature requests discussion](https://github.com/org-roam/org-roam-ui/discussions/66) or upvote those already posted, this way we can adjust our priorities somewhat!

### Q: This doesn't work with org-roam v1/org-brain/Zettledelft!

Correct! We only support org-roam v2!

## Features ✨

Org-Roam-UI's main feature is the ability to generate a graph visualization of your org-roam notes.

### Cool graph

![image](https://user-images.githubusercontent.com/21983833/127747037-aac46e8a-8617-4436-8887-ea1ad7a3141a.png)

#### Sliders!

Configure the graph just the way you like it.

![image](https://user-images.githubusercontent.com/21983833/127746907-f6283400-c7b9-432f-bd85-e37b16a753e9.png)

#### Colors

A ton

![image](https://user-images.githubusercontent.com/21983833/127747009-a4be5e47-1df6-4311-a747-71613168d028.png)

or just a splash.

![image](https://user-images.githubusercontent.com/21983833/127746962-97ed15d2-ffeb-4a3b-9743-ad77e7efce2e.png)

#### Open notes in Emacs

(Double) clicking a node will open the corresponding note in Emacs, very cool.
You _don't_ need org-protocol for this, it works out of the box!

https://user-images.githubusercontent.com/21983833/127747170-3b49fbde-7fc5-410f-bd26-4ffea8dae48c.mp4

#### Follow your movement in Emacs!

When you open a note in Emacs, org-roam-ui will move to the corresponding node on the graph.

https://user-images.githubusercontent.com/21983833/127747187-7823a825-a2f8-449c-a0ec-1c5c525621dc.mp4

#### Theme syncing

Your gruvbox is only a `M-x orui-sync-theme` away.

https://user-images.githubusercontent.com/21983833/127747203-a1aeca95-7def-4caf-b2cf-8a18fa1f2059.mp4

#### Filters

https://user-images.githubusercontent.com/21983833/127747222-7c768e37-4948-4072-895b-a55ff7b5906a.mp4

#### 3 D

Literally deepen your understanding of your thoughts (and it looks cool)

https://user-images.githubusercontent.com/21983833/127747234-d0588cdf-623f-4d13-a060-737bc570b295.mp4

## Planned features

### Graph

In no particular order

- ~~Citation links + customization~~
- ~~Tag filtering/coloring~~
- Local graph show Nth neighbor
- More colors
- Colorization options (by neighbors, centrality, etc)
- Setting profiles

### UI in general

- File viewing using AST parsing
- Displaying notes Andy Matushak style
- Discovery options, e.g. "show shortest path between X and Y"

### Beyond

- Discuss the future of org-roam-ui with us [here!](https://github.com/org-roam/org-roam-ui/discussions/6)

# Contribute 💪

```bash
git clone https://github.com/org-roam/org-roam-ui
yarn
yarn dev
```

[GitHub Community Guidelines
](https://docs.github.com/en/github/site-policy/github-community-guidelines) apply.

Go [here](https://github.com/org-roam/org-roam-ui/discussions/4) to have an onboarding call with a member of the core team.

We would ❤️ to have you on board.
