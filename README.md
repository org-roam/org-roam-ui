# org-roam-ui: An org-roam frontend

(cool picture)

Org-Roam-UI is a frontend for exploring and interacting with your [org-roam](https://github.com/org-roam/org-roam) notes.

Org-Roam-UI is meant a successor of https://github.com/org-roam/org-roam-server that extends functionality of org-roam with a Web app that runs side-by-side with Emacs.

## Features ✨

Org-Roam-UI's main feature is the ability to generate a graph visualization of your org-roam notes.

### Cool graph

#### Sliders!

Configure the graph just the way you like it.

#### Colors

A ton, or just a splash.

#### Open notes in Emacs

(Double) clicking a node will open the corresponding note in Emacs, very cool.

#### 3 D

Literally deepen your understanding of your thoughts (and it looks cool)

#### Follow your movement in Emacs, and back!

When you open a note in Emacs, org-roam-ui will move to the corresponding node on the graph.

#### Theme syncing

Your gruvbox is only a `M-x org-roam-ui-sync-theme` away.

## Installation

`org-roam-ui` is not (yet!) on MELPA.

Should your Emacs miss any of the dependencies, please install them manually!
org-roam-ui requires `org-roam`, `websocket`, `simple-httpd`, `f` and Emacs >27 for fast JSON parsing.

### Prerequisites

The graph utilizes `org-protocol`, which means if you click on one
of the nodes, it will open the corresponding file in Emacs. For this
feature to work, org-roam protocol should be configured in the system.

[Configuring Org-Roam Protocol](https://www.orgroam.com/manual.html#Installation-_00281_0029)

Also make sure the emacs server is started; `M-x server-start RET`

### Manually

Install `websocket.el`

```emacs-lisp
M-x package-install websocket
```

Clone the repo:

```bash
cd ~/.emacs.d/private
git clone git@github.com:org-roam/org-roam-ui.git
```

Load in Emacs (add to config):

```lisp
(require 'websocket)
(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
(load-library "org-roam-ui")
```

### Doom

Add the following to your `package.el`

```emacs-lisp
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo org-roam/org-roam-ui :files "*.el out")
```

Then something along the following to your `config.el`

```emacs-lisp
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :hook (org-roam . org-roam-ui-mode)
    :config
)

```

We recommend only loading org-roam-ui after loading org(-roam) as starting the server and making database requests can impact startup times quite a lot.

### Quelpa/use-package

TODO
You probably know how to do this

## Usage

Use `M-x org-roam-ui RET` to enable the global mode.
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
    (list
        (bg . '#1E2029')
        (bg-alt . '#282a36')
        (fg . '#f8f8f2')
        (fg-alt . '#6272a4')
        (red . '#ff5555')
        (orange . '#f1fa8c')
        (yellow .'#ffb86c')
        (green . '#50fa7b')
        (cyan . '#8be9fd')
        (blue . '#ff79c6')
        (violet . '#8be9fd')
        (magenta . '#bd93f9')))
```

You can optionally provide `(base1 . '#XXXXXX')` arguments after the last one to also set the background shades, otherwise ORUI will guess based on the provides bg and fg.

## Disclaimers ‼

- We only support [org-roam v2](https://blog.jethro.dev/posts/org_roam_v2/); v1 will never be supported.
- As the name suggests, Org-Roam-UI only works with org-roam! If you organize your notes in some other form org-roam-ui cannot work, as it uses org-roam to fetch all the connections.
- Feature-parity with org-roam-server is not the goal. Although we aim to make a similar product which is having a visual graph to help you explore and navigate your org-roam nodes, we do not intend to replicate all of the, nor be limited to replicating the features of org-roam-server.
- This is alpha software: please do give it a try and use it, but expect bugs and troubleshooting!
- The project was created by a couple of tinkerers to scratch their own itch. We don't get rewarded in any material way and development may stop any day (because life). The best way to keep the project alive is to [explore the code and contribute](#Contribute)!

## FAQ 🗨

### Q: Graph Slow! Faster?

While we try to optimize the display of the graph, there is only so much we can do. For largish networks (>2k nodes) dragging the graph around a lot can cause some performance issues, but there are a few things you can do to speed it up.

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

### Q: Some of my links are not showing up?

At the moment we aren't showing citation links made with `org-roam-bibtex` yet, but we will soon!

### Q: Will you implement X?

Hopefully, yeah! But time is limited, and so is the amount of features we can cram into this things before it implodes in itself, so we are adding things incrementally to make sure they work. That said, we'd love to hear from you! Please post feature requests in [this discussion]() or upvote those already posted, this way we can adjust our priorities somewhat!

### Q: This doesn't work with org-roam v1/org-brain/Zettledelft!

Correct! We only support org-roam v2!

## Planned features

### Graph

In no particular order

- Citation links + customization
- Tag filtering/coloring
- Local graph show Nth neighbor
- More colors
- Colorization options (by neighbors, centrality, etc)
-

### UI in general

- File viewing using AST parsing
- Displaying notes Andy Matushak style
- Discovery options, e.g. "show shortest path between X and Y"

### Beyond

- Discuss the future of org-roam-ui with us (here!)[https://github.com/org-roam/org-roam-ui/discussions/6]

# Contribute 💪

[GitHub Community Guidelines
](https://docs.github.com/en/github/site-policy/github-community-guidelines) apply.

Go [here](https://github.com/org-roam/org-roam-ui/discussions/4) to have an onboarding call with a member of the core team.

We would ❤️ to have you on board.
