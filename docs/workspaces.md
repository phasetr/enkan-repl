# Workspace Design for enkan-repl

## Goals

- Support multiple concurrently active projects by introducing a workspace concept.
- Keep current user flows intact; add workspace as an opt-in layer.
- Maintain compatibility with existing buffer naming and tests where no workspace is selected.

## Current Architecture Overview

- Buffer naming: enkan buffers are named as `*enkan:<expanded-path>*` via `enkan-repl--make-buffer-name` and parsed using regexes like `^\\*enkan:` and `^\\*enkan:\\(.*\\)\\*$`.
- Global state (single-project oriented):
  - `enkan-repl--current-project`
  - `enkan-repl-session-list` (alist `(number . project-name)`)
  - `enkan-repl--session-counter`
  - `enkan-repl-project-aliases` (defcustom)
- Setup/teardown:
  - `enkan-repl-setup` handles two modes:
    - Standard input file: split windows and start one eat session for the current directory.
    - Center file: select a project from `enkan-repl-projects`, reset globals, set aliases, start sessions from `enkan-repl-target-directories`, and set `enkan-repl--current-project`.
  - `enkan-repl-teardown`:
    - Standard input file: terminate a single session for the current directory.
    - Center file: terminate all registered sessions and reset global state.
- Send operations resolve a target directory/alias and locate the eat buffer by exact name equality with `enkan-repl--make-buffer-name`. Matching and extraction depend on the `*enkan:<path>*` pattern.

## Problem Statement

- The single set of global variables and strict buffer name matching limit the system to effectively one concurrently active “project layout”.
- We want multiple independent “layouts” (sets of sessions and project bindings) and a way to switch among them.

## Proposed Workspace Model

### Identifiers

- Workspace ID: ゼロ埋め数値の文字列（例: `01`, `02`）。バッファ名では `ws:<id>` として使用し、内部状態のキーにも同一の `<id>` を用いる。

### State Structure

- New variables:
  - `enkan-repl--current-workspace` (string or nil): current workspace ID.
  - `enkan-repl--workspaces` (alist): `(workspace-id . plist)` where plist contains:
    - `:current-project` (string or nil)
    - `:session-list` (alist `(number . project-name)`)
    - `:session-counter` (integer)
    - `:project-aliases` (alist `(alias . project-name)`)

- Shadow (compatibility) globals remain and mirror the current workspace:
  - `enkan-repl--current-project`
  - `enkan-repl-session-list`
  - `enkan-repl--session-counter`
  - `enkan-repl-project-aliases`

- On workspace switch:
  1. Save the current globals into the plist for `enkan-repl--current-workspace` in `enkan-repl--workspaces`.
  2. Load the target workspace’s plist back into the globals and set `enkan-repl--current-workspace`.

This keeps existing functions unchanged while making their effects workspace-scoped through the shadowed globals.

## Buffer Naming and Parsing

### Formats

- Legacy (no workspace): `*enkan:/abs/path/to/project*`
- Workspace-prefixed: `*ws:01 enkan:/abs/path/to/project*`（ID は 2 桁ゼロ埋め）

### Matching and Extraction

- Update regexes to受け入れる: `enkan:` の前に必ず `ws:<nn>`（`nn` は 2 桁ゼロ埋め数値）が付く前提に変更。
  - Start-of-name check: `^\\*ws:[0-9]{2} enkan:`
  - Path capture (group 1): `^\\*ws:[0-9]{2} enkan:\\(.*?\\)\\*$`

- Update helpers accordingly:
- `enkan-repl--make-buffer-name`: 常に `*ws:<nn> enkan:<path>*` を生成（当面は単一 WS のため `<nn>` は `01`）。
  - `enkan-repl--buffer-matches-directory`, `enkan-repl--extract-directory-from-buffer-name`, `enkan-repl--extract-project-name`, and any code scanning by `^\\*enkan:` should use the optional-prefix-aware regex.

### Scoping by Workspace

- `enkan-repl--get-buffer-for-directory` should match only buffers for the current workspace if one is selected (i.e., name begins with `*<current-ws> enkan:`). In legacy mode (no current workspace), it should match only legacy names (no workspace prefix) to avoid cross-workspace collisions.

## Workspace Commands

- `enkan-repl-workspace-create`:
  - 新しいワークスペース ID は手入力させず、自動採番（`01` からのゼロ埋め増分）。
  - `enkan-repl--workspaces` に空の plist を作成し、直ちにカレント WS に切替。

- `enkan-repl-workspace-switch`:
  - Prompt from existing workspace IDs (using `hmenu`/`completing-read`).
  - Save current globals to the current workspace plist, then load the target workspace’s plist into globals.

- `enkan-repl-workspace-close`:
  - Confirm termination of all sessions in that workspace.
  - Kill matching buffers, remove workspace from `enkan-repl--workspaces`.
  - If it was the current workspace and others remain, prompt for the next current workspace.

## Integration with Setup/Teardown

- `enkan-repl-setup` (center-file mode):
  - In addition to selecting a project, also prompt/select a workspace (default to `enkan-repl--current-workspace`, or create one if none).
  - Perform the current setup steps, but write results to the current workspace’s state (through the shadow globals).

- `enkan-repl-teardown` (center-file mode):
  - Terminate only the sessions associated with the current workspace and clear its state.
  - Optionally provide a separate command for “terminate all workspaces” if needed; keep default teardown scoped.

- Standard input file behavior remains unchanged and operates within the currently selected workspace.

## Migration & Compatibility

- レガシー `*enkan:/...*` 形式は新仕様では非対応。移行ガイドで一括置換/再生成手順を提供する。
- 既存テストは新形式 `*ws:<nn> enkan:/...*` に更新する。

## Edge Cases and Risks

- Buffer collisions: With workspace prefixes, two workspaces can target the same directory without buffer name clashes.
- Cross-workspace bleed: Ensure buffer discovery is filtered by the current workspace where applicable.
- State drift on switch: Provide atomic helpers for save/load of workspace state into/from globals.
- Center-file detection remains the same; document that setup/teardown now operate per current workspace in that mode.

## Implementation Plan

この章の段階計画は下記「PR ベースの段階的リファクタリング計画」に置き換える。

## Testing Plan

- Parsing and naming:
  - `enkan-repl--extract-directory-from-buffer-name` は `*ws:01 enkan:/...*` を扱う（テストは新形式のみ）。
  - `enkan-repl--extract-project-name` は新形式の最終ディレクトリ名を返す。
  - `enkan-repl--buffer-matches-directory` は現 WS にスコープしたバッファのみ一致させる。

- Buffer discovery:
  - `enkan-repl--get-buffer-for-directory` finds the correct buffer in current workspace; does not cross-match into other workspaces.

- Workspace state:
  - Switching serializes current globals into `enkan-repl--workspaces` and restores target workspace state accurately.

- Setup/teardown:
  - Center-file setup binds project and sessions per workspace.
  - Teardown only closes sessions for the current workspace.

-- Legacy compatibility: 対応しない（移行ガイドで代替）。

## Future Enhancements (Optional)

- Minor-mode lighter showing current workspace (e.g., `[ws:01]`).
- Auto-generate next numeric ID (e.g., `02`, `03`).
- Command to list all workspaces and jump/close from a menu.

## リファクタリング計画（実装前の準備）

目的: ワークスペース機能を非破壊・段階的に導入できるよう、バッファ名の扱いとグローバル状態の依存を最小化・一元化する。

方針概要
- 名称/正規表現の一元化: `*enkan:<path>*` の判定/抽出/生成はヘルパ関数だけを経由し、直書きの正規表現を排除する。
- レガシー互換の保持: `enkan-repl--current-workspace` が `nil` の場合は現行挙動を完全維持（バッファ名に WS プレフィクスなし、既存テストは壊さない）。
- 状態のカプセル化: 将来の WS 切替に備え、グローバル変数の保存/復元用フック（未実装のスタブ）を追加して呼び出し点を固定化する。

追加/変更する内部 API（第1段階・互換モードのみ）
- `enkan-repl--current-workspace`（変数・初期値 nil）: WS 選択がない限り未使用。
- `enkan-repl--is-enkan-buffer-name (name)`: enkan バッファ名か判定（WS 省略可）。
- `enkan-repl--buffer-name->path (name)`: バッファ名からフルパスを抽出。
- `enkan-repl--path->buffer-name (path)`: パスからレガシー形式のバッファ名を生成（後続で WS 対応に差し替え）。
- `enkan-repl--buffer-regex ()`（必要なら）: 将来 WS プレフィクスを含む正規表現を一元提供。

直書き正規表現の置換対象（代表箇所）
- `enkan-repl.el`
  - L484, L653, L1343, L1540 の `^\*enkan:` 判定を `enkan-repl--is-enkan-buffer-name` に置換。
  - L1229–1243 の `^\*enkan:\(.+\)\*$` 抽出を `enkan-repl--buffer-name->path` に置換。
  - `enkan-repl--get-buffer-for-directory` は `enkan-repl--path->buffer-name` を使った完全一致に統一。
- `enkan-repl-utils.el`
  - 既存の `enkan-repl--extract-directory-from-buffer-name` / `enkan-repl--make-buffer-name` を内部で新 API を呼ぶ薄いラッパに変更（互換維持）。

セッション探索/列挙の整備
- `enkan-repl--get-available-buffers` は `enkan-repl--is-enkan-buffer-name` を使用。
- `enkan-repl--buffer-matches-directory` は `enkan-repl--path->buffer-name` による完全一致で判定（正規表現ロジックを集約）。

状態管理の準備（スタブ追加のみ）
- `enkan-repl--save-workspace-state` / `enkan-repl--load-workspace-state`（未実装・no-op）を追加し、後続で WS 切替時に実装可能な呼び出し点を `setup/teardown` の直前後に挿入。

テスト戦略
- 既存 ERT が緑のままになるよう、初期段階ではレガシー互換に限定。
- 追加予定のユニットテスト（後続フェーズ）:
  - `test/enkan-repl-workspace-utils-test.el`: 判定/抽出/生成ヘルパの純粋関数テスト。
  - バッファ列挙のフィルタがヘルパ経由になっていることのスモークテスト。

移行手順（小さな PR 単位）
1) ヘルパ API 追加（既存実装のレガシー互換ラッパ）。
2) 代表的な呼び出し点を置換（抽出系→列挙系→UI系の順）。
3) 直書き正規表現ゼロを確認（`rg "^\\*enkan:"` などで検査）。
4) `make check` で回帰を確認。

完了条件
- 直書きの `^\*enkan:` / `^\*enkan:\(.+\)\*$` が 0 件。
- `make check` が通る（テスト・lint・byte-compile・checkdoc・format）。
- 以後、WS 対応の差分は `enkan-repl--current-workspace` とヘルパ実装の差し替えで最小化可能。

リスク/対策
- リスク: 呼び出し点の置換漏れによるテスト不安定化。
  - 対策: `rg` で網羅的にヒット箇所を抽出し、段階的に置換 → `make check` を都度実行。
- パフォーマンス影響は軽微（単純な文字列比較/正規表現をヘルパに集約するのみ）。

備考
- この段階では WS 機能自体は追加しない（API の差し替え土台のみ）。
- 実装フェーズでは、ここで導入したヘルパに WS プレフィクス対応を追加し、`enkan-repl--get-buffer-for-directory` などに WS スコープを適用する。

## 破壊的変更方針（WS 前提・単一 WS 仮運用）

- ポリシー: 今後は「ワークスペースありき」の設計に一本化する。完全実装が揃うまでは「単一ワークスペース（`ws:01`）のみ存在する」前提でパッケージ全体を動作させる。
- 互換性: 旧来のバッファ命名/グローバル変数は互換維持しない（移行ガイド提供）。
- リスク低減: 小さな PR を積み上げ、各 PR に明確な完了条件とテスト追加を伴わせる。

## PR ベースの段階的リファクタリング計画（小粒・順序固定）

以下は 1 PR = 1 目的に極小化したプラン。各 PR は必ず `make check` 完走と ERT 追加を含むこと。

PR-0: WS 基盤スケルトン導入（動作不変）
- 目的: 以降の変更点の受け皿を用意する。
- 変更: `enkan-repl--current-workspace`（デフォルト `"ws:01"`）、`enkan-repl--workspaces`（`(workspace-id . plist)`）を追加。セッション関連のアクセサ群（getter/setter）を追加（まだ既存変数の裏で同値に動く）。
- テスト: 新規アクセサの入出力が既存グローバルと同値であること。
- 受け入れ基準: 既存テスト緑/警告増加なし。

PR-1: コアの状態参照をアクセサ経由に置換（副作用なし）
- 変更: `enkan-repl.el` 内の `enkan-repl-session-list` / `enkan-repl--session-counter` / `enkan-repl--current-project` / `enkan-repl-project-aliases` 直接参照をアクセサに置換（フォーマット系・一覧系など安全領域から）。
- テスト: 影響関数のスモークを追加（表示文字列の同一性）。
- 受け入れ基準: diff は参照置換のみ、挙動/文言不変。

PR-2: セッション探索/送信経路をアクセサ化
- 変更: `enkan-repl--get-buffer-for-directory`/`enkan-repl--get-available-buffers`/送信系ユニファイド関数の内部でアクセサ使用に統一。
- テスト: 送信成功/失敗パスの回帰テストを追加。
- リスク: 隠れた直接参照の取りこぼし。→ `rg` で網羅チェック。

PR-3: バッファ名 API の確立（旧 API は内部委譲）
- 変更: `enkan-repl--is-enkan-buffer-name` / `enkan-repl--buffer-name->path` / `enkan-repl--path->buffer-name` を導入。既存 `enkan-repl--extract-directory-from-buffer-name` / `enkan-repl--make-buffer-name` は内部で新 API を呼ぶ薄いラッパに変更。
- テスト: 旧/新 API が同一結果を返すこと（この PR では命名様式はまだ旧）。

PR-4: WS 固定前提の命名様式へ切替（破壊的変更）
- 変更: バッファ名を必ず `*ws:<nn> enkan:/abs/path*` 形式に変更（当面は単一 WS のため `<nn>` は `01` 固定運用）。正規表現/抽出/生成を新 API に一本化。既存の `^\*enkan:` マッチは全面撤去。
- テスト: すべてのテストデータを新命名に更新。抽出/一致/列挙のカバレッジ強化。
- ドキュメント: README/Examples のスクリーンショット/例文を更新。

PR-5: セッション探索の WS スコープ化
- 変更: `enkan-repl--get-buffer-for-directory`/`enkan-repl--get-available-buffers` が「現在の WS のみ」を対象にする。内部的には「バッファ名の WS トークン一致」を強制。
- テスト: 異なる WS トークンを含むフェイクバッファが混在しても、現 WS のみヒットする。

PR-6: setup/teardown の WS 前提化
- 変更: `enkan-repl-setup`/`enkan-repl-teardown` が必ず「現在の WS」を読み書きする。センターファイル/通常ファイルの分岐説明を WS 観点で再記述。現段階では WS は 1 つだけなので UI は増やさない。
- テスト: teardown が現 WS 配下のセッションだけを終了させること。

PR-7: 旧来グローバルの直参照を全面廃止
- 変更: ファイル全域から直参照を削除し、アクセサ経由に統一。不要になった互換コードを削除。
- テスト: ビルド警告（未束縛変数参照など）が 0。既存テスト緑。

PR-8: 内部状態保存/復元（WS 1個用の実装）
- 変更: `enkan-repl--save-workspace-state` / `enkan-repl--load-workspace-state` を実装。現状は単一 WS だが API は多 WS 拡張可能な形に。
- テスト: 保存→復元でセッションリスト/カウンタ/別名が等価になる。

PR-9: ドキュメント/移行ガイドの確定
- 変更: 破壊的変更点（命名様式変更・旧グローバル非推奨→廃止）を明記。サンプルを全更新。
- 受け入れ基準: `make docs` 成功、README/examples/workspaces.md に差分が反映。

PR-10: レガシーラッパー関数の削除（破壊的変更）
- 変更: PR-3で薄いラッパーに置換した以下の関数を削除
  - `enkan-repl--extract-directory-from-buffer-name`: 新API `enkan-repl--buffer-name->path` を直接使用
  - `enkan-repl--make-buffer-name`: 新API `enkan-repl--path->buffer-name` を直接使用
- 移行: 全コードベースで旧関数の呼び出しを新APIに置換
- テスト: 旧関数への参照が0件であることを確認（`rg`で検証）
- 受け入れ基準: `make check` 緑、旧関数名の参照が残っていないこと

## 命名と正規表現（新仕様）

- バッファ名: `*ws:<nn> enkan:<abs-path>*` を唯一の正規形式とする（`nn` は 2 桁ゼロ埋め、例: `01`）。
- 判定: `^\*ws:[0-9]{2} enkan:`
- 抽出（パス）: `^\*ws:[0-9]{2} enkan:\(.*?\)\*$`
- 生成: `enkan-repl--path->buffer-name` が必ず WS トークンを前置して生成。

## 品質ゲートとレビューチェックリスト（各 PR 共通）

- make タスク: `make test && make compile && make checkdoc && make lint && make format`
- 探索: `rg "^\\*enkan:"` / `rg "^\\*ws:"` で直書き/置換漏れを可視化。
- テスト: 新旧経路での ERT 差分を確認し、回帰防止テストを追加。
- コミット: Conventional Commits に準拠（例: `refactor(ws): introduce buffer-name api`）。

## リスクと緩和策（再掲）

- 漏れによるセッション特定失敗 → 正規表現/生成の一元化とユニットテストで防止。
- レビュー負荷 → PR 小粒化・明確な完了条件・ログ/ドキュメント更新の徹底。
- ドキュメント乖離 → 各 PR で README/examples の必要部分を必ず更新。

## 実装進捗と次の作業（2025-09-06 時点）

### 完了済みPR

#### ✅ PR-0: WS基盤スケルトン導入
- **実装内容**:
  - `enkan-repl--current-workspace`（デフォルト `"01"`）
  - `enkan-repl--workspaces`（`(workspace-id . plist)`）
  - 保存・復元スタブ関数
- **影響**: 挙動不変、既存テスト緑
- **マージ済み**: main ブランチ

#### ✅ PR-1: WSアクセサ導入（読取側）
- **実装内容**:
  - アクセサAPI追加: `enkan-repl--ws-{current-project,session-list,session-counter,project-aliases}`
  - セッターAPI追加: `enkan-repl--ws-set-*` 系
  - 読取箇所の置換完了
- **影響**: 挙動不変、既存テスト緑
- **マージ済み**: main ブランチ

#### ✅ PR-2: WSアクセサ導入（書込側）
- **実装内容**:
  - 書込箇所を setter に置換
  - register-session、setup系、リセット関数の更新
- **影響**: 挙動不変、既存テスト緑
- **マージ済み**: main ブランチ

#### ✅ PR-3: バッファ名API導入（互換モード）
- **実装内容**:
  - 新API追加:
    - `enkan-repl--is-enkan-buffer-name`: バッファ名判定
    - `enkan-repl--buffer-name->path`: パス抽出
    - `enkan-repl--path->buffer-name`: バッファ名生成
  - 既存関数を薄いラッパーに変更:
    - `enkan-repl--extract-directory-from-buffer-name`
    - `enkan-repl--make-buffer-name`
  - テスト追加: `test/enkan-repl-name-api-test.el`
- **影響**: 挙動不変、全テスト緑（77/77 pass）
- **マージ済み**: main ブランチ (commit `86285d5`)

#### ✅ PR-4: 命名仕様切替（破壊的変更）
- **実装内容**:
  - バッファ名形式を `*ws:01 enkan:/path*` に変更
  - 全てのバッファ名APIを新形式に対応:
    - `enkan-repl--path->buffer-name`: 必ず `ws:01` プレフィクスを付与
    - `enkan-repl--buffer-name->path`: 新形式の正規表現 `^\\*ws:[0-9]\\{2\\} enkan:`
    - `enkan-repl--is-enkan-buffer-name`: 新形式のみを認識
  - 全コードベースの正規表現を更新（10ファイル、187行変更）
  - 全テストファイルを新形式に更新
- **影響**: **破壊的変更** - 既存の `*enkan:/path*` 形式のバッファは再作成が必要
- **マージ済み**: main ブランチ (commit `f9d4298`)

#### ✅ PR-5: セッション探索のWSスコープ化
- **実装内容**:
  - `enkan-repl--get-buffer-for-directory`: 現在のWSのバッファのみを返すよう実装済み
  - `enkan-repl--get-available-buffers`: WSトークンでフィルタリング実装済み
  - `enkan-repl--buffer-name-matches-workspace`: WSマッチング関数追加
  - `enkan-repl--extract-workspace-id`: WS ID抽出関数追加
  - テスト追加: `test/enkan-repl-workspace-scope-test.el`
- **影響**: セッション探索が現在のWS（`ws:01`）に限定される
- **マージ済み**: main ブランチ (commit `948db4f`)

#### ✅ PR-6: setup/teardownのWS前提化
- **実装内容**:
  - `enkan-repl-setup`/`enkan-repl-teardown`: docstringにWSコンテキストを明記
  - `enkan-repl--terminate-all-session-buffers`: WSスコープの説明を追加
  - docstring警告修正（80文字制限）
  - テスト追加: `test/enkan-repl-workspace-setup-teardown-test.el`
- **影響**: setup/teardown操作が現在のWSに限定される
- **マージ済み**: main ブランチ (PR #33)

#### ✅ PR-7: 旧来グローバルの直参照を全面廃止
- **実装内容**:
  - enkan-repl.el, enkan-repl-utils.el内のフォーマット文字列を修正
    - 変数名の表示を内部実装に依存しない名前に変更
  - examples/window-layouts.elをアクセサ経由に修正
    - require 'enkan-repl を追加
    - `enkan-repl--current-project` → `(enkan-repl--ws-current-project)`
    - `enkan-repl-session-list` → `(enkan-repl--ws-session-list)`
  - テストファイルは内部実装テストのため例外として維持
- **影響**: 外部からの旧来グローバル変数への直接参照が廃止
- **マージ済み**: PR #34 (commit `671ff25`)

### 次の作業: PR-8（内部状態保存/復元）

**目的**: ワークスペース切替のための状態保存/復元機能を実装

**作業内容**:
- `enkan-repl--save-workspace-state` / `enkan-repl--load-workspace-state` を実装
- 現状は単一 WS だが API は多 WS 拡張可能な形に

**完了条件**:
- 保存→復元でセッションリスト/カウンタ/別名が等価になる
- 既存テスト緑
