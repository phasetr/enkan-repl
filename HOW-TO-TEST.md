# マイナーモード・チートシート機能の試用手順

## 前提条件

現在の調査用ブランチ `feature/minor-mode-cheatsheet-research` で実装済み.

## 試用手順

### 1. パッケージの読み込み

```bash
# Emacsを起動
emacs

# 以下をEmacsで実行 (M-: で eval-expression)
(progn
  (add-to-list 'load-path "/Users/sekine/dev/self/claudemacs-repl")
  (require 'claudemacs-repl))
```

### 2. org-modeファイルを開く

```bash
# 既存のtest-org.orgを開く
C-x C-f /Users/sekine/dev/self/claudemacs-repl/test-org.org
```

または新しいorg-modeファイルを作成:
```bash
C-x C-f test.org
```

### 3. マイナーモードの動作確認

org-modeファイルを開くと:
- **モードライン**に ` ClaudeREPL` が表示されることを確認
- これが表示されていればマイナーモードが自動有効化されている

### 4. チートシート機能の試用

#### 基本的な呼び出し
```
C-c C-h
```

すると以下が表示される:
- **プロンプト**: `claudemacs-repl commands: `
- **候補一覧**: 18個の公開関数がリスト表示
- **アノテーション**: 各関数の右側にdocstring表示

#### 実際の表示例
```
claudemacs-repl commands: 
claudemacs-repl-send-region — Send the text in region from START to END to claudemacs.
claudemacs-repl-send-buffer — Send the entire current buffer to claudemacs.
claudemacs-repl-send-rest-of-buffer — Send rest of buffer from cursor position to end to claudemacs.
...
```

#### 検索・絞り込み
- **部分検索**: `send` と入力すると send系の関数のみ表示
- **複数キーワード**: `buffer send` で buffer を含む send 関数を絞り込み
- **TAB補完**: TAB で補完候補の循環

#### 実行
- **Enter**: 選択した関数を実行
- **C-g**: キャンセル

### 5. キーバインド優先度の確認

他のマイナーモードやメジャーモードでC-c C-hが使われていても, claudemacs-repl-modeが優先されることを確認:

```emacs-lisp
# 他のC-c C-hバインドが設定されている状態でテスト
(local-set-key (kbd "C-c C-h") 'describe-mode)  # 通常はこれが呼ばれる
# しかしorg-mode + claudemacs-repl-modeでは claudemacs-repl-cheatsheet が呼ばれる
```

### 6. org-mode以外での無効化確認

```bash
# 他のモードのファイルを開く
C-x C-f test.txt
```

- **確認事項**: モードラインに ` ClaudeREPL` が**表示されない**こと
- **確認事項**: `C-c C-h` でチートシートが**呼ばれない**こと

### 7. 手動でのマイナーモード切り替え

```
M-x claudemacs-repl-mode
```

- org-mode以外では無効化される
- org-modeでは有効化される

## トラブルシューティング

### チートシートが空の場合

```emacs-lisp
# デバッグ用コード
(let ((funcs (claudemacs-repl-utils--extract-function-info 
              (expand-file-name "claudemacs-repl.el" 
                               "/Users/sekine/dev/self/claudemacs-repl"))))
  (message "Found %d functions" (length funcs)))
```

### マイナーモードが有効化されない場合

```emacs-lisp
# 確認コード
(message "claudemacs-repl-mode: %s" claudemacs-repl-mode)
(message "major-mode: %s" major-mode)
(message "org-mode-p: %s" (derived-mode-p 'org-mode))
```

### キーバインドが効かない場合

```emacs-lisp
# キーマップ確認
(message "Key binding: %s" 
         (lookup-key claudemacs-repl-mode-map (kbd "C-c C-h")))
(message "emulation-mode-map-alists: %s" 
         (assq 'claudemacs-repl-mode (car emulation-mode-map-alists)))
```

## 完全な試用シナリオ

1. `emacs` 起動
2. `M-: (add-to-list 'load-path "/Users/sekine/dev/self/claudemacs-repl")`
3. `M-: (require 'claudemacs-repl)`  
4. `C-x C-f test.org` (新規org-modeファイル作成)
5. モードラインで ` ClaudeREPL` 確認
6. `C-c C-h` でチートシート起動
7. `send` と入力して絞り込み確認
8. `claudemacs-repl-send-region` を選択してEnter
9. 関数が実行されることを確認

これで全機能の動作確認が完了します.