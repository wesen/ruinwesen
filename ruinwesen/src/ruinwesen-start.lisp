(require :asdf)
(push "/home/manuel/asds/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op 'swank)

; start swank
(swank:create-server :dont-close t :port 4006)

(asdf:oos 'asdf:load-op :ruinwesen)

(ruinwesen:startup)


