(ecb-layout-define "burke" left-right ""
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (previous-window))
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window (next-window (next-window))))
  (ecb-set-speedbar-buffer)
  (select-window (previous-window (selected-window)) 0))

(provide 'ecb-layout-burke)