;;-------------------------------------------------------------------
;; treeview.l - treeview.dll 使用サンプル
;;-------------------------------------------------------------------

;; Copyright (C) 2002 kazu.y
;;

;;--------------------
;;  概要
;;--------------------
;;
;;  このファイルは TreeCtrl を xyzzy に埋め込んで使用するためのライブラリ
;;  TreeView.dll を扱うサンプルです。
;;
;;  # 行儀が悪い＆使いづらいので、このファイルを直接使わずに同等機能の
;;    独自のラッパを書いてアプリケーションを開発したほうがよいと思います。
;;
;;  # TreeView.dll がエクスポートしている関数の一覧および簡単な説明は
;;    readme.txt にあります。

;;--------------------
;;  使用方法
;;--------------------
;;
;;  treeview.dll を xyzzy と同じディレクトリに、listview.l を site-lisp/ におき、
;;  listview.l をロード (load-library treeview) した後、例えば以下のような
;;  コードを実行します。
;;
;;  ; サンプルその1
;;  ; # 終了は (treeview-close-sample1) です。
;;  (defvar tviewid1 nil)
;;  (defun treeview-sample1 ()
;;    (interactive)
;;    (setq tviewid1 (treeview-create))
;;    ; ダブルクリックされたときに情報をステータスバーに表示する
;;    (treeview-dblclk-callback tviewid1 #'(lambda (hitem)
;;      (message (format nil "Double Click!! Text = ~A HasChildren = ~A"
;;        (treeview-get-item-text tviewid1 hitem)
;;        (treeview-dll-item-has-children tviewid1 hitem)))))
;;    ; アイテムの追加
;;    (let ((fluits (treeview-insert-item tviewid1 "フルーツ" TVI_ROOT)))
;;      (treeview-insert-item tviewid1 "りんご" fluits)
;;      (treeview-insert-item tviewid1 "ばなな" fluits))
;;    (let ((vegetables (treeview-insert-item tviewid1 "野菜" TVI_ROOT)))
;;      (treeview-insert-item tviewid1 "キャベツ" vegetables)
;;      (treeview-insert-item tviewid1 "トマト" vegetables)))
;;  
;;  (defun treeview-close-sample1 ()
;;    (interactive)
;;    (treeview-close tviewid1))
;;  
;;  
;;  ; サンプルその2
;;  ; # 終了は (treeview-close-sample2) です。
;;  (c:*define VK_ESCAPE     #x1B)
;;  ;(c:define-dll-entry winapi:HWND SetFocus (winapi:HWND) "user32")
;;  (defvar tviewid2 nil)
;;  (defun treeview-sample2 ()
;;    (interactive)
;;    ; TreeView の作成
;;    (setq tviewid2 (treeview-create))
;;    ; スタイルをいろいろ変更
;;    (treeview-dll-modify-style tviewid2 0
;;      (logior TVS_HASBUTTONS TVS_HASLINES TVS_LINESATROOT) 0)
;;    ; ESC で xyzzy にフォーカスを移す
;;    (treeview-keydown-callback tviewid2 #'(lambda (hitem vkey flag)
;;      (if (= vkey VK_ESCAPE) (SetFocus (get-window-handle)))))
;;    ; アイコンの登録
;;    (let ((open-icon  (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\open.ico"))
;;          (close-icon (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\close.ico"))
;;          (file-icon  (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\file.ico")))
;;      ; アイテムの追加
;;      (let ((hedder (treeview-insert-item tviewid2 "ヘッダーファイル" TVI_ROOT :icon close-icon :sicon open-icon)))
;;        (treeview-insert-item tviewid2 "hoge.h" hedder :icon file-icon :sicon file-icon)
;;        (treeview-insert-item tviewid2 "fugo.h" hedder :icon file-icon :sicon file-icon)
;;        ; ツリーを展開
;;        (treeview-dll-expand tviewid2 hedder TVE_EXPAND))
;;      (let ((source (treeview-insert-item tviewid2 "ソースファイル" TVI_ROOT :icon close-icon :sicon open-icon)))
;;        (treeview-insert-item tviewid2 "hoge.cpp" source :icon file-icon :sicon file-icon)
;;        (treeview-insert-item tviewid2 "fugo.cpp" source :icon file-icon :sicon file-icon)
;;        ; ツリーを展開
;;        (treeview-dll-expand tviewid2 source TVE_EXPAND))))
;;  
;;  (defun treeview-close-sample2 ()
;;    (interactive)
;;    (treeview-close tviewid2))


(require "foreign")
(require "wip/winapi")

;;-----------------
;; 雑多な定義類
;;-----------------

; treeview-create-exのflags
(c:*define XPIS_LEFT    0)
(c:*define XPIS_TOP     1)
(c:*define XPIS_RIGHT   2)
(c:*define XPIS_BOTTOM  3)
(c:*define XPIS_NOMOVE  4)
(c:*define XPIS_POSMASK 7)
(c:*define XPIS_ORDMASK #x30)
(c:*define XPIS_OUTSIDE #x10)
(c:*define XPIS_INSIDE  #x20)
(c:*define XPIS_NOORDER #x30)
(c:*define XPIS_GROUP   #x40)

; 型の定義
(c:*define-c-type winapi:LPSTR    LPTSTR)
(c:*define-c-type winapi:LPCSTR   LPCTSTR)
(c:*define-c-type (winapi:UINT *) PUINT)
(c:*define-c-type winapi:HANDLE   HTREEITEM)

(c:*define-c-struct TVITEM
  (winapi:UINT    mask           )
  (HTREEITEM      hItem          )
  (winapi:UINT    state          )
  (winapi:UINT    stateMask      )
  (LPTSTR         pszText        )
  (c:int          cchTextMax     )
  (c:int          iImage         )
  (c:int          iSelectedImage )
  (c:int          cChildren      )
  (winapi:LPARAM  lParam         ))

; スタイル設定用 - ModifyStyle ()
(c:*define TVS_HASBUTTONS       #x0001)
(c:*define TVS_HASLINES         #x0002)
(c:*define TVS_LINESATROOT      #x0004)
(c:*define TVS_EDITLABELS       #x0008)
(c:*define TVS_DISABLEDRAGDROP  #x0010)
(c:*define TVS_SHOWSELALWAYS    #x0020)
(c:*define TVS_RTLREADING       #x0040)
(c:*define TVS_NOTOOLTIPS       #x0080)
(c:*define TVS_CHECKBOXES       #x0100)
(c:*define TVS_TRACKSELECT      #x0200)
(c:*define TVS_SINGLEEXPAND     #x0400)
(c:*define TVS_INFOTIP          #x0800)
(c:*define TVS_FULLROWSELECT    #x1000)
(c:*define TVS_NOSCROLL         #x2000)
(c:*define TVS_NONEVENHEIGHT    #x4000)
(c:*define TVS_NOHSCROLL        #x8000)

; hitem の代わりに使える - InsertItem ()等
(c:*define TVI_ROOT       (- 0 #x10000))
(c:*define TVI_FIRST      (- 0 #x0FFFF))
(c:*define TVI_LAST       (- 0 #x0FFFE))
(c:*define TVI_SORT       (- 0 #x0FFFD))

; 変更箇所のマスク - InsertItem (), SetItem ()
(c:*define TVIF_TEXT          #x0001)
(c:*define TVIF_IMAGE         #x0002)
(c:*define TVIF_PARAM         #x0004)
(c:*define TVIF_STATE         #x0008)
(c:*define TVIF_HANDLE        #x0010)
(c:*define TVIF_SELECTEDIMAGE #x0020)
(c:*define TVIF_CHILDREN      #x0040)

; 状態変更用 - InsertItem ()等
(c:*define TVIS_SELECTED        #x0002)
(c:*define TVIS_CUT             #x0004)
(c:*define TVIS_DROPHILITED     #x0008)
(c:*define TVIS_BOLD            #x0010)
(c:*define TVIS_EXPANDED        #x0020)
(c:*define TVIS_EXPANDEDONCE    #x0040)
(c:*define TVIS_OVERLAYMASK     #x0F00)
(c:*define TVIS_STATEIMAGEMASK  #xF000)
(c:*define TVIS_USERMASK        #xF000)
(c:*define TVIS_EXPANDPARTIAL   #x0080)

; Expand () 用
(c:*define TVE_COLLAPSE       #x0001)
(c:*define TVE_EXPAND         #x0002)
(c:*define TVE_TOGGLE         #x0003)
(c:*define TVE_COLLAPSERESET  #x8000)
(c:*define TVE_EXPANDPARTIAL  #x4000)

; GetNextItem (), Select ()
(c:*define TVGN_ROOT             #x0000)
(c:*define TVGN_NEXT             #x0001)
(c:*define TVGN_PREVIOUS         #x0002)
(c:*define TVGN_PARENT           #x0003)
(c:*define TVGN_CHILD            #x0004)
(c:*define TVGN_FIRSTVISIBLE     #x0005)
(c:*define TVGN_NEXTVISIBLE      #x0006)
(c:*define TVGN_PREVIOUSVISIBLE  #x0007)
(c:*define TVGN_DROPHILITE       #x0008)
(c:*define TVGN_CARET            #x0009)
(c:*define TVGN_LASTVISIBLE      #x000A)

;;-----------------
;; 実装
;;-----------------

;; 関数のインポート

(let ((dll "TreeView.dll"))
  (c:define-dll-entry c:int treeview-dll-create (winapi:HWND (c:void *)) dll "Create")
  (c:define-dll-entry c:int treeview-dll-create-ex (winapi:HWND (c:void *) c:int winapi:DWORD) dll "CreateEx")
  (c:define-dll-entry c:int treeview-dll-close (c:int) dll "Close")
  (c:define-dll-entry winapi:HWND treeview-dll-get-hwnd (c:int) dll "GetHwnd")
  (c:define-dll-entry winapi:BOOL treeview-dll-setsize (c:int c:int c:int c:int c:int) dll "SetSize")
  (c:define-dll-entry winapi:BOOL treeview-dll-setpos (c:int winapi:DWORD) dll "SetPos")

  (c:define-dll-entry winapi:HWND treeview-dll-get-htree (c:int) dll "GetHtree")
  (c:define-dll-entry winapi:BOOL treeview-dll-modify-style (c:int winapi:DWORD winapi:DWORD winapi:UINT) dll "ModifyStyle")

  (c:define-dll-entry HTREEITEM treeview-dll-get-child-item (c:int HTREEITEM) dll "GetChildItem")
  (c:define-dll-entry winapi:UINT treeview-dll-get-count (c:int) dll "GetCount")
  (c:define-dll-entry winapi:BOOL treeview-dll-get-item (c:int (TVITEM *)) dll "GetItem")
  (c:define-dll-entry winapi:DWORD treeview-dll-get-item-data (c:int HTREEITEM) dll "GetItemData")
  (c:define-dll-entry winapi:UINT treeview-dll-get-item-state (c:int HTREEITEM winapi:UINT) dll "GetItemState")
  (c:define-dll-entry winapi:BOOL treeview-dll-get-item-text (c:int HTREEITEM LPTSTR c:int) dll "GetItemText")
  (c:define-dll-entry HTREEITEM treeview-dll-get-next-item (c:int HTREEITEM winapi:UINT) dll "GetNextItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-next-sibling-item (c:int HTREEITEM) dll "GetNextSiblingItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-parent-item (c:int HTREEITEM) dll "GetParentItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-prev-sibling-item (c:int HTREEITEM) dll "GetPrevSiblingItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-root-item (c:int) dll "GetRootItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-selected-item (c:int) dll "GetSelectedItem")
    (c:define-dll-entry winapi:BOOL treeview-dll-item-has-children (c:int HTREEITEM) dll "ItemHasChildren")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item
    (c:int HTREEITEM winapi:UINT LPCTSTR c:int c:int winapi:UINT winapi:UINT winapi:LPARAM) dll "SetItem")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-data (c:int HTREEITEM winapi:DWORD) dll "SetItemData")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-state (c:int HTREEITEM winapi:UINT winapi:UINT) dll "SetItemState")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-text (c:int HTREEITEM LPCTSTR) dll "SetItemText")
  (c:define-dll-entry winapi:BOOL treeview-dll-delete-all-items (c:int) dll "DeleteAllItems")
  (c:define-dll-entry winapi:BOOL treeview-dll-delete-item (c:int HTREEITEM) dll "DeleteItem")
  (c:define-dll-entry winapi:HWND treeview-dll-edit-label (c:int HTREEITEM) dll "EditLabel")
  (c:define-dll-entry HTREEITEM treeview-dll-insert-item
    (c:int winapi:UINT LPCTSTR c:int c:int winapi:UINT winapi:UINT winapi:LPARAM HTREEITEM HTREEITEM) dll "InsertItem")
  (c:define-dll-entry winapi:BOOL treeview-dll-select (c:int HTREEITEM winapi:UINT) dll "Select")
  (c:define-dll-entry winapi:BOOL treeview-dll-expand (c:int HTREEITEM winapi:UINT) dll "Expand")
  (c:define-dll-entry winapi:UINT treeview-dll-sort-children (c:int HTREEITEM) dll "SortChildren")
  
  (c:define-dll-entry winapi:BOOL treeview-dll-set-click-callback (c:int (c:void *)) dll "SetClickCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-rclick-callback (c:int (c:void *)) dll "SetRclickCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-dblclk-callback (c:int (c:void *)) dll "SetDblclkCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-keydown-callback (c:int (c:int *)) dll "SetKeyDownCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-edit-label-callback (c:int (c:void *)) dll "SetEditLabelCallback")
  
  (c:define-dll-entry c:int treeview-dll-add-file-icon (c:int LPCTSTR) dll "AddFileIcon")
  (c:define-dll-entry c:int treeview-dll-add-icon (c:int winapi:HICON) dll "AddIcon")
  (c:define-dll-entry winapi:BOOL treeview-dll-remove-icon (c:int c:int) dll "RemoveIcon")
  (c:define-dll-entry winapi:BOOL treeview-dll-remove-all-icons (c:int) dll "RemoveAllIcons")

  (c:define-dll-entry winapi:BOOL treeview-dll-disable-char-jump (c:int winapi:BOOL) dll "DisableCharJump")

  (c:define-dll-entry c:int treeview-dll-simple-track-popup-menu (LPCTSTR) dll "SimpleTrackPopupMenu"))

;; (treeview-create)
;; <概要>
;;   開く
;; <引数>
;; <戻り値>
;; <解説>
(defun treeview-create ()
  (treeview-dll-create (get-window-handle) (si:plugin-arg)))

;; (treeview-create-ex (size 400) (pos XPIS_LEFT))
;; <概要>
;; 位置、サイズを指定して開く
;; <引数>
;; <戻り値>
;; <解説>
(defun treeview-create-ex (&optional (size 400) (pos XPIS_LEFT))
  (treeview-dll-create-ex (get-window-handle) (si:plugin-arg) size pos))

;; (treeview-close viewid)
;; <概要>
;;   閉じる
;; <引数>
;; <戻り値>
;; <解説>
(defun treeview-close(viewid)
  (treeview-dll-close viewid))

;; (treeview-setpos lviewid size min max step)
;; <概要>
;;   ビューをリサイズする
;; <引数>
;; <戻り値>
;; <解説>
;;   stepは1でいい気がします
(defun treeview-setsize(viewid size min max step)
  (treeview-dll-setsize viewid size min max step))

;; (treeview-close lviewid)
;; <概要>
;;   位置を変更する
;; <引数>
;;   pos - treeview-create-exのposと同じ
;; <戻り値>
;; <解説>
(defun treeview-setpos(viewid pos)
  (treeview-dll-setpos viewid pos))

;; (treeview-insert-item viewid item-index text :icon :id)
;; <概要>
;; アイテムを挿入する
;; <引数>
;; <戻り値>
;; <解説>
(defun treeview-insert-item (viewid text parent &key icon sicon)
  (let ((imask TVIF_TEXT))
    (if (null icon ) (setq icon  0) (setq imask (logior imask TVIF_IMAGE)))
    (if (null sicon) (setq sicon 0) (setq imask (logior imask TVIF_SELECTEDIMAGE)))
    (treeview-dll-insert-item
      viewid imask (si:make-string-chunk text) icon sicon 0 0 0 parent 0)))

;; (treeview-set-item-text viewid hitem text)
;; <概要>
;;   アイテムのテキストを設定する
;; <引数>
;; <戻り値>
;; <解説>
(defun treeview-set-item-text (viewid hitem text)
  (treeview-dll-set-item-text viewid hitem (si:make-string-chunk text)))

;; (treeview-get-item-text viewid hitem (maxlen 100))
;; <概要>
;;   アイテムのテキストを取得する
;; <引数>
;;   maxlen は最大取得可能文字列。
;; <戻り値>
;; <解説>
(defun treeview-get-item-text (viewid hitem &optional (maxlen 100))
  (let ((text (si:make-chunk nil maxlen)))
    (treeview-dll-get-item-text viewid hitem text maxlen)
    (si:unpack-string text 0)))

;; (treeview-click-callback viewid fn)
;; <概要>
;;   クリック時のコールバック
;; <引数>
;; <戻り値>
;; <解説>
(let (callback)
  (c:defun-c-callable c:void treeview-click-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-click-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-click-callback viewid #'treeview-click-callback-imp)))))

;; (treeview-dblclk-callback viewid fn)
;; <概要>
;;   ダブルクリック時のコールバック
;; <引数>
;; <戻り値>
;; <解説>
(let (callback)
  (c:defun-c-callable c:void treeview-dblclk-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-dblclk-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-dblclk-callback viewid #'treeview-dblclk-callback-imp)))))

;; (treeview-rclick-callback viewid fn)
;; <概要>
;;   右クリック時のコールバック
;; <引数>
;; <戻り値>
;; <解説>
(let (callback)
  (c:defun-c-callable c:void treeview-rclick-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-rclick-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-rclick-callback viewid #'treeview-rclick-callback-imp)))))

;; (treeview-keydown-callback viewid fn)
;; <概要>
;;   キーダウン時のコールバック
;; <引数>
;; <戻り値>
;; <解説>
(let (callback)
  (c:defun-c-callable c:void treeview-keydown-callback-imp ((HTREEITEM hitem) (winapi:WORD vkey) (winapi:UINT flag))
    (funcall callback hitem vkey flag))
  (defun treeview-keydown-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-keydown-callback viewid #'treeview-keydown-callback-imp)))))

;; (treeview-edit-label-callback viewid fn)
;; <概要>
;;   ラベル編集後のコールバックを設定する
;; <引数>
;; <戻り値>
;; <解説>
;;   ラベル編集後に入力された文字列をチェックしたい場合に利用します。
;;   コールバックを設定すると編集結果が TreeCtrl に自動的に反映されなくなります。
(let ((callback) (maxlen 100))
  (c:defun-c-callable c:void treeview-edit-label-callback-imp ((HTREEITEM hitem) (LPCTSTR text))
    (funcall callback hitem (si:unpack-string (si:make-chunk nil maxlen nil text) 0)))
  (defun treeview-edit-label-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-edit-label-callback viewid #'treeview-edit-label-callback-imp)))))

;; (treeview-set-focus viewid)
;; <概要>
;;   TreeCtrl にフォーカスを移す
;; <引数>
;; <戻り値>
;; <解説>
;;  逆は下のほうのドキュメント参照
(c:define-dll-entry winapi:HWND SetFocus (winapi:HWND) "user32")
(defun treeview-set-focus (viewid)
  (interactive)
  (SetFocus (treeview-dll-get-hwnd viewid)))

#|
;; TreeCtrl にフォーカスがあるときにESCキーでフォーカスを xyzzy に戻す
;; ためのサンプル
(c:*define VK_ESCAPE     #x1B)
(treeview-keydown-callback viewid #'(lambda (index vkey flag)
  (if (= vkey VK_ESCAPE) (SetFocus (get-window-handle)))))
|#

;; (treeview-add-file-icon viewid filepath)
;; <概要>
;;   アイコンファイルを登録する
;; <引数>
;; <戻り値>
;;   アイコンの登録番号
;; <解説>
;;   アイコンの登録番号は使用するときのために記憶しておく。
(defun treeview-add-file-icon (viewid filepath)
  (treeview-dll-add-file-icon viewid (si:make-string-chunk filepath)))

;; (treeview-set-item-icon viewid hitem icon sicon)
;; <概要>
;;   アイテムのアイコンを変更する
;; <引数>
;;   icon アイコンの登録番号
;; <戻り値>
;; <解説>
(defun treeview-set-item-icon (viewid hitem icon sicon)
  (treeview-dll-set-item viewid hitem
    (logior TVIF_IMAGE TVIF_SELECTEDIMAGE) 0 icon sicon 0 0 0))

;; (treeview-simple-track-popup-menu menu-format)
;; <概要>
;;   ポップアップメニューを表示する
;; <引数>
;;   menu-format
;;     例) "開く(&O)\n0\n削除(&D)\n1\nsep\n2048\nプロパティ(&P)\n0"
;;     "開く(&O)\n0\n"で1つのメニュー項目を表す。
;;     "sep\n2048\n"でセパレータ。
;;     "削除(&D)\n1\n"でアイテムをグレー表示。
;;     数字の意味は以下参照。
;;      #define MF_SEPARATOR        0x00000800L
;;      #define MF_ENABLED          0x00000000L
;;      #define MF_GRAYED           0x00000001L
;;      #define MF_DISABLED         0x00000002L
;;      #define MF_UNCHECKED        0x00000000L
;;      #define MF_CHECKED          0x00000008L
;;      #define MF_USECHECKBITMAPS  0x00000200L
;;      #define MF_STRING           0x00000000L
;; <戻り値>
;;   選択されたメニューの位置を返す
;; <解説>
;;    もともとyzzy関数でポップアップメニューをビュー上に表示しようとすると、
;;    表示位置がマウスカーソルの位置からずれてしまったためにこの関数を実装
;;    しましたが、どうやらdefine-popup-menuとtrack-popup-menuで正しくメニュー
;;    ポップアップができるみたいです。
;;    というわけで、複雑な(に限らず)メニューのポップアップをするには、
;;    この関数を使わず、以下のコードを用いることを推奨します。
;;      (defun create-abc-menu ()
;;        (define-popup-menu
;;          (:item nil "ほげ(&H)"
;;           'hoge 100)
;;          :sep
;;          (:item nil "もにょ(&M)"
;;           'monyo)))
;;      (setq *abc-menu* (create-abc-menu))
;;      (track-popup-menu *abc-menu* :button2)
(defun treeview-simple-track-popup-menu (menu-format)
  (treeview-dll-simple-track-popup-menu (si:make-string-chunk menu-format)))

#|
アイテムをソートする方法
ListView とは異なり、Win32APIおよびMFCに標準で辞書順ソートする関数があります。

  (treeview-dll-sort-children viewid item)
|#

#|
英数字キーを押したときにその文字から始まるアイテムにフォーカスが移動してしまう
のを抑止する方法。

  (treeview-dll-disable-char-jump viewid 1)
|#
