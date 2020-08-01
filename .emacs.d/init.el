;; 対応する括弧を強調表示
(show-paren-mode t)
;; 括弧を補完する
(electric-pair-mode 1)

;; 時間も表示
(display-time)

;; ~からはじまるバックアップファイルを作らない
(setq make-backup-files nil)
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
(defun count-line)

