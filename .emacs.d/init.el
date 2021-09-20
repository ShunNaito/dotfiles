;; ⌘キーをメタキーに割り当てる
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

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

;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
;; (global-linum-mode t)
(global-display-line-numbers-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; インデントの設定                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; タブの表示幅。初期値は8
(setq-default tabwidth 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

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
(set-face-background 'region "darkgreen")

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

;;(require 'git-gutter)

;; If you enable global minor mode
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  ;; linum-modeを利用している場合は次の設定も追加
  ;; (git-gutter:linum-setup)
  )
