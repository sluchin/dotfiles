# プロジェクト設定: dotfiles

環境設定ファイル（dotfiles）リポジトリ。Emacs、Zsh、tmux、X11 などの各種設定およびユーティリティスクリプトを管理する。

## コミットガイドライン

- コミットメッセージは英語で記述
- **件名は、Conventional Commits の形式で書く**: `type(scope): description`。scope (括弧を含む) は、省略できる。description は、英語で、命令形 (例: `feat(emacs): Add keybinding for dired`)。
  - type は、次のどれか。
    - `feat`: 機能や設定の追加
    - `fix`: バグや設定の修正
    - `docs`: 文書とコメントだけの変更 (`*.md` など)
    - `style`: 整形、インデント・空白の調整 (動作を変えない)
    - `refactor`: 設定やスクリプトの整理・書き換え (動作を変えない)
    - `test`: テストの追加・変更
    - `build`: ビルドやセットアップ環境 (`Makefile`、`setup.sh` など) の変更
    - `ci`: CI の変更
    - `chore`: そのほかの雑務 (`.gitignore` など)
  - scope は、変更の対象を表す短い名前 (例: `emacs`、`zsh`、`tmux`、`x11`、`bin`、`git`)。
  - 互換性を壊す変更は、type (または scope) の直後に、感嘆符を付ける (Conventional Commits の仕様のとおり)。
  - 複数の type にまたがるときは、変更の中心になる type にする。
- 件名のあとに空行を 1 行入れて、本文を書く。本文はダッシュ/ハイフンで始まる箇条書き（例: `- Fix bug in parser`）
- コミットメッセージに `Co-Authored-By` または Claude 帰属行を含めない
- **rebase したあとの push は、`git push --force-with-lease --force-if-includes origin <ブランチ名>` で行う** (`--force` は使わない。リモートに、手元に取り込んでいないコミットがあれば、push は失敗する)。ユーザーに依頼されたときだけ、push する。

## タスク完了時の動作

- 修正が完了したら、`git diff --stat` で差分を確認し、変更点と理由の簡潔なまとめをユーザーに報告する。
- ユーザーに依頼されない限り、`git commit` / `git push` は行わない。

## 開発ルール

- Markdown (`*.md`) の日本語の文章は、句読点に「、」「。」を使う (「,」「.」は使わない)。インラインコード・コードブロック・URL・ファイル名・数値 (`1.2.0`、`1,000`) は対象外。見出しや番号付きリストの「1.」の形は変えない。この `CLAUDE.md` は対象外。
