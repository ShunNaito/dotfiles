;;; 対応する括弧を強調表示
;; (show-paren-mode t)

;; 時間も表示
(display-time)

;; ⌘キーをメタキーに割り当てる
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; キーボードマクロの練習
(fset 'insert-
      "\C-a-\C-n")

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; （カスタムファイルが存在しない場合は作成する）
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

;; Macだけに読み込ませる内容を書く
(when (eq system-type 'darwin)
  ;; MacのEmacsでファイル名を正しく扱うための設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; ターミナル以外はツールバー、スクロールバーを非表示
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0)
  ;; scroll-barを非表示
  (scroll-bar-mode 0))

;; CocoaEmacs以外はメニューバーを非表示
(unless (eq window-system 'ns)
  ;; menu-barを非表示
  (menu-bar-mode 0))

;; Emacs 23より前のバージョンを利用している方は
;; user-emacs-directory変数が未定義のため次の設定を追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; cl-libパッケージを読み込む
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインドの設定                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 文字列（"\C-m"）よりもキーシーケンス((kbd "C-m"))の視認性に優れるというメリットがある
;; global-set-keyはdefine-key global-mapの短縮表記のようなもの

;; C-mにnewline-and-indentを割り当てる。
(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-hを<DEL>（バックスペース）に置き換える
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; 別のキーバインドにヘルプを割り当てる
(define-key global-map (kbd "C-x ?") 'help-command)

;; 折返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境変数の設定                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; パスの設定
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/local/bin")

;; 文字コードの指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フレームに関する設定                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行番号/カラム番号を表示する
(column-number-mode t)

;; 行番号を表示させない
(line-number-mode 0)

;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示
;; (setq display-time-day-and-date 0) ; 曜日・月・日を表示
;; (setq display-time-24hr-formt 0) ; 24時間表示
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; リージョン内の行数と文字数をモードラインに表示する
(defun count-lines-and-chars ()
  (if mark-active
      (format "(%dlines,%dchars) "
	      (count-lines (region-beginning) (region-end))
	      (- (region-end) (region-beginning)))
    ""))

;; emacs26からdefault-*の変数が使えなくなった
;; (add-to-list 'default-mode-line-format
;; 	     '(:eval (count-lines-and-chars)))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; インデントの設定                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; タブの表示幅。初期値は8
(setq-default tabwidth 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; php-modeのみタブを利用しない
(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bsd")))

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    表示・装飾に関する設定                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; リージョンの背景色を変更
;; (set-face-background 'region "darkgreen")

;; AsciiフォントをMenloに
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)

;; 日本語フォントをNoto Serif CJK JPに
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Noto Serif CJK JP"))

;; ひらがなとカタカナをNoto Sans CJK JPに
;; U+3000-303F CJKの記号および句読点
;; U+3040-309F ひらがな
;; U+30A0-30FF カタカナ
(set-fontset-font
 nil '(#x3040 . #x30ff)
 (font-spec :family "Noto Sans CJK JP"))

;; Notoフォントの横幅を調整
(add-to-list 'face-font-rescale-alist '(".*Noto.*" . 1.2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ハイライトの設定                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青に
    (((class color) (background light))
     (:background "LightSkyBlue" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode :対応するカッコを強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t)       ;有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
;;(set-face-background 'show-paren-match-face nil)
(set-face-background 'show-paren-match nil)
(set-face-underline-p 'show-paren-match "darkgreen")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    バックアップとオートセーブ                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; バックアップファイルを作成しない
;; (setq make-backup-files nil)            ;初期値はt
;; オートセーブファイルを作らない
;; (setq aut-save-default nil)             ;初期値はt

;; 括弧を補完する
;; (electric-pair-mode 1)

;; バックアップファイルの作成場所をシステムのTempディレクトリに変更する
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; オートセーブファイルの作成場所をシステムのTempディレクトリに変更する
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; オートセーブファイル作成までの秒間隔
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    変更されたファイルの自動更新                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    フック                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ファイルが #! から始まる場合、+xをつけて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp-modeのフックをセット
;; (add-hook 'emacs-lisp-mode-hook
;;          '(lamba ()
;; (when (require 'eldoc nil t)
;;   (setq eldoc-idle-delay 0.2)
;;   (setq eldoc-echo-are-use-multiline-p t)
;;                    (turn-on-eldoc-mode))))

;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-are-use-multiline-p t)
    (turn-on-eldoc-mode)))

;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Elispをインストールしよう                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; undo-treeパケージを読み込む
(require 'undo-tree)

(require 'package)                      ;package.elを有効化
;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)                    ;インストール済みのElispを読み込む

;; zenburnテーマを利用する
(load-theme 'zenburn t)

;; Helm
(require 'helm)
(require 'helm-config)

(require 'helm-descbinds)
(helm-descbinds-mode)

;; M-yにhelm-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;; color-moccurの設定
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; moccur-editの設定
(require 'moccur-edit nil t)

;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-change-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; wgrepの設定
(require 'wgrep nil t)

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  ;; C-'にリドゥを割り当てる
  ;; (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))

;; point-undo MELPAから削除されてる

;; ElScreenのプレフィックスキーを変更する（初期値はC-z）
;; (setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  (elscreen-start)
  ;; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconfify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; hoemメモ保存の場所
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを1日1ファイルにする
                                        ; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; howmメモを保存と同時に閉じる
(defun howm-save-buffer-and-kill ()
  "howmメモを保存と同時に閉じます。"
  (interactive)
  (when (and (buffer-file-name)
             (howm-buffer-p))
    (save-buffer)
    (kill-buffer nil)))

;; C-c C-cでメモの保存と同時にバッファを閉じる
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; cua-modeの設定
(cua-mode t)                            ;cua-modeをオン
(setq cua-enable-cua-keys nil)          ; CUAキーバインドを無効にする

(when (require 'web-mode nil t)
  ;; 自動的にweb-modeを起動したい拡張子を追加する
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;;; web-modeのインデント設定用フック
  ;; (defun web-mode-hook ()
  ;;   "Hooks for Web mode. "
  ;;   (setq web-mode-markup-indent-offset 2) ; HTMLのインデント
  ;;   (setq web-mode-css-indent-offset 2) ; CSSのインデント
  ;;   (setq web-mode-code-indent-offset 2) ;JS, PHP, Rubyなどのインデント
  ;;   (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
  ;;   (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
  ;;   (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
  ;;   )
  ;; (add-hook 'web-mode-hook 'web-mode-hook)
  )

;; HTML編集のデフォルトモードをnxml-modeにする
;; (add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . nxml-mode))

;; </を入力すると自動的にタグを閉じる
(setq nxml-slash-auto-complete-flag t)
;; M-TABでタグを保管する
(setq nxml-bind-meta-tab-to-complete-flag t)
;; nxml-modeでauto-complete-modeを利用する
(add-to-list 'ac-modes 'nxml-mode)

;;  子要素のインデント幅を設定する。初期値は2
(setq nxml-child-indent 0)
;; 属性値のインデント値を設定する。初期値は4
(setq nxml-attribute-indent 0)

;; スペース2つのインデントにしたい場合の設定
;; (defun js-indent-hook ()
;;   ;; インデント幅を2にする
;;   (set js-indent-level 2
;;        js-expr-indent-offset 2
;;        indent-tabs-mode nil)
;;   ;; switch文のcaseラベルをインデントする関数を定義する
;;   (defun my-js-indent-line ()
;;     (interactive)
;;     (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
;;            (offset (- (current-column) (current-indentation)))
;;            (indentation (js-properindentation parse-status)))
;;       (back-to-indentation)
;;       (if (looking-at "case\\s-")
;;           (indent-line-to (+ indentation 2))
;;         (js-indent-line))
;;       (when (> offset 0) (forward-char offset))))
;;   ;; caseラベルのインデント処理をセットする
;;   (set (make-local-variable 'indent-line-function) 'my-js-indent-line)
;;   ;; ここまでcaseラベルを調整する設定
;;   )

;; ;; js-modeの起動時にhookを追加
;; (add-hook 'js-mode-hook 'js-indent-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; React (JSX)を使う場合はこちら
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

;; 日本語ドキュメントを利用するための設定
(when (require 'php-mode nil t)
  (setq php-site-url "https://secure.php.net/"
        php-manual-url 'ja))

;; php-modeのインデント設定
(defun php-indent-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)               ;インデントを2文字にしたい場合2にする
  ;; (c-set-offset 'case-label '+) ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧

(add-hook 'php-mode-hook 'php-indent-hook)

;; perl-modeをcperl-modeのエイリアスにする
(defalias 'perl-mode 'cperl-mode)

;; dtwをdelete-trailing-whitespaceのエイリアスにする
(defalias 'dtw 'delete-trailing-whitespace)

;; cperl-modeのインデント設定
(setq cperl-indent-level 4              ;インデント幅を4にする
      cperl-continued-statement-offset 4 ;継続する文のオフセット
      cperl-brace-offset -4              ;ブレースのオフセット
      cperl-label-offset -4              ;labelのオフセット
      cperl-indent-parens-as-block t     ;カッコもブロックとしてインデント
      cperl-close-paren-offset -4        ;閉じカッコのオフセット
      cperl-tab-always-indent t          ;tabをインデントにする
      cperl-highlight-variables-indiscriminately t) ;スカラを常にハイライト

;; ruby-modeのインデント設定
(setq ruby-indent-level 3               ;インデント幅を3に。初期値は2
      ruby-deep-indent-paren-style nil  ;改行時のインデントを調整する
      ;; ruby-mode実行時にindent-tabs-modeを設定値に変更
      ruby-indent-tabs-mode t)          ;タブ文字を使用する。初期値はnil

;; ruby-mode-hookにruby-electric-modeを追加
(add-hook 'ruby-mode-hook #'ruby-electric-mode)

(setq python-check-command "flake8")    ;標準ではpyflakes

;; Flycheckによる文法チェックをオンにする
(add-hook 'after-init-hook #'global-flycheck-mode)

;; エラーをツールチップとして表示する
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;; gtags-modeのキーバインドを有効化する
(setq gtags-suggested-key-mapping t)    ;無効化する場合はコメントアウト
;; ファイル保存時に自動的にタグをアップデートする
(setq gtags-auto-update t)              ;無効化する場合はコメントアウト

(custom-set-variables
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-auto-update t))

;; projectile
(when (require 'projectile nil t)
  ;; 自動的にプロジェクト管理を開始
  (projectile-mode)
  ;; プロジェクト管理から除外するディレクトリを追加
  (add-to-list
   'projectile-globally-ignored-directories
   "node_modules")
  ;; プロジェクト情報をキャッシュする
  (setq projectile-enable-caching t))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; Fuzzyマッチを無効にする。
;; (setq helm-projectile-fuzzy-match nil)
(when (require 'helm-projectile nil t)
  (setq projectile-completion-system 'helm))

;; projectile-railsのプレフィックスキーをs-rに変更
;; (setq projectile-rails-keymap-prefix (kbd "s-r")
(when (require 'projectile-rails nil t)
  (projectile-rails-global-mode))
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

(when (require 'ac-emoji nil t)
  ;; text-modeとmarkdwon-modeでauto-completeを有効にする
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'markdown-mode)
  ;; text-modeとmarkdown-modeでac-emojiを有効にする
  (add-hook 'text-mode-hook 'ac-emoji-setup)
  (add-hook 'markdown-mode-hook 'ac-emoji-setup))

;; ediffコントロールパネルを別フレームにしない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; SQLサーバへ接続するためのデフォルト情報
;; (setq sql-user "root"                   ;デフォルトユーザー名
;;       sql-database "database_name"      ;データベース名
;;       sql-server "localhost"            ;ホスト名
;;       sql-product 'mysql)               ;データベースの種類

