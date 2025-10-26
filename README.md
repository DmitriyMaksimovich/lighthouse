# Lighthouse

 This package provides functionality to highlight multiple lines in an Emacs buffer. 
 Highlights persist across buffer switches and are removed when the buffer is killed. 
 The highlight color can be customized via `lighthouse-highlight-color'. 
 Commands: 
 - lighthouse-toggle: Toggle highlight on the current line. 
 - lighthouse-clear-all: Remove all highlights in the current buffer. 
 - lighthouse-next: Jump to the next highlighted line, cycling to the first if at the last. 
 - lighthouse-prev: Jump to the previous highlighted line, cycling to the last if at the first. 

## How to use (doom emacs guide)
### Add the package in `packages.el`

```el
(package! lighthouse
  :recipe (:host github :repo "your-username/lighthouse"))
```

### Configurate in `config.el`
#### Enable in programming modes

```el
(use-package! lighthouse
  :hook (prog-mode . lighthouse-mode) ; Enable in programming modes
  :config
  (setq lighthouse-highlight-color "#504945"))
```

#### Optionally, configurate keybindings

```el
(with-eval-after-load 'lighthouse
  (define-key lighthouse-mode-map (kbd "C-c h") #'lighthouse-toggle)
  (define-key lighthouse-mode-map (kbd "C-c c") #'lighthouse-clear-all)
  (define-key lighthouse-mode-map (kbd "C-c n") #'lighthouse-next)
  (define-key lighthouse-mode-map (kbd "C-c p") #'lighthouse-prev))
```

### Run `doom sync`
