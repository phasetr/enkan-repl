#!/bin/bash
# scripts/quality-check.sh

set -e

echo "🔍 claudemacs-client 品質チェック開始"
echo "========================================"

# 色付きメッセージ用の関数
print_success() { echo -e "\033[32m✅ $1\033[0m"; }
print_warning() { echo -e "\033[33m⚠️  $1\033[0m"; }
print_error() { echo -e "\033[31m❌ $1\033[0m"; }

# 1. バイトコンパイル
echo ""
echo "📦 バイトコンパイルチェック"
echo "--------------------------------"

BYTE_COMPILE_OUTPUT=$(emacs --batch --eval '(byte-compile-file "claudemacs-client.el")' 2>&1)
BYTE_COMPILE_EXIT_CODE=$?

if [ $BYTE_COMPILE_EXIT_CODE -eq 0 ]; then
    if [ -n "$BYTE_COMPILE_OUTPUT" ]; then
        print_warning "バイトコンパイル完了（警告あり）"
        echo "$BYTE_COMPILE_OUTPUT"
    else
        print_success "バイトコンパイル完了（警告なし）"
    fi
else
    print_error "バイトコンパイル失敗"
    echo "$BYTE_COMPILE_OUTPUT"
    exit 1
fi

# 2. checkdoc
echo ""
echo "📝 checkdocチェック"
echo "--------------------------------"

CHECKDOC_OUTPUT=$(emacs --batch --eval '(checkdoc-file "claudemacs-client.el")' 2>&1)
CHECKDOC_EXIT_CODE=$?

if [ $CHECKDOC_EXIT_CODE -eq 0 ]; then
    if [ -n "$CHECKDOC_OUTPUT" ]; then
        print_warning "checkdoc完了（警告あり）"
        echo "$CHECKDOC_OUTPUT"
    else
        print_success "checkdoc完了（警告なし）"
    fi
else
    print_error "checkdoc失敗"
    echo "$CHECKDOC_OUTPUT"
    exit 1
fi

# 3. 基本機能テスト
echo ""
echo "🧪 基本機能テスト"
echo "--------------------------------"

BASIC_TEST_OUTPUT=$(emacs --batch -l claudemacs-client.el --eval '(claudemacs-client-version)' 2>&1)
BASIC_TEST_EXIT_CODE=$?

if [ $BASIC_TEST_EXIT_CODE -eq 0 ]; then
    print_success "基本機能テスト完了"
    echo "バージョン出力: $BASIC_TEST_OUTPUT"
else
    print_error "基本機能テスト失敗"
    echo "$BASIC_TEST_OUTPUT"
    exit 1
fi

# 4. ファイル存在チェック
echo ""
echo "📂 必須ファイルチェック"
echo "--------------------------------"

REQUIRED_FILES=("claudemacs-client.el" "README.md" "LICENSE" "CHANGELOG.md")
MISSING_FILES=()

for file in "${REQUIRED_FILES[@]}"; do
    if [ -f "$file" ]; then
        print_success "$file 存在"
    else
        print_warning "$file 未作成"
        MISSING_FILES+=("$file")
    fi
done

# 5. 結果サマリー
echo ""
echo "📊 品質チェック結果サマリー"
echo "========================================"

if [ ${#MISSING_FILES[@]} -eq 0 ] && [ -z "$BYTE_COMPILE_OUTPUT" ] && [ -z "$CHECKDOC_OUTPUT" ]; then
    print_success "すべてのチェックがクリア！"
    echo "🚀 公開準備完了状態です"
elif [ ${#MISSING_FILES[@]} -gt 0 ]; then
    print_warning "未作成ファイルがあります: ${MISSING_FILES[*]}"
    echo "📝 ドキュメント整備が必要です"
elif [ -n "$BYTE_COMPILE_OUTPUT" ] || [ -n "$CHECKDOC_OUTPUT" ]; then
    print_warning "警告があります"
    echo "🔧 コード品質向上が推奨されます"
else
    print_success "基本的な品質基準をクリア"
    echo "✨ 継続的な改善を続けましょう"
fi

echo ""
echo "📋 次のアクション:"
echo "  1. 警告の修正（推奨）"
echo "  2. 未作成ファイルの作成"
echo "  3. プルリクエスト作成"
echo "  4. バージョン更新 (0.0.0 → 0.0.1)"