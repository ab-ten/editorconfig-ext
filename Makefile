.PHONY: test test-interactive clean byte-compile help

# ローカル設定ファイルがあれば読み込み
-include Makefile.local

# デフォルト設定
EMACS ?= emacs
BATCH_OPTS ?= --batch --no-init-file --no-site-file

# テストファイル
TEST_FILE = test/editorconfig-ext-test.el
SOURCE_FILE = editorconfig-ext.el

# Emacsコマンドの構築
EMACS_CMD = $(EMACS) $(BATCH_OPTS)

help: ## ヘルプを表示
	@echo "Available targets:"
	@echo "  test            - バッチモードでテスト実行"
	@echo "  test-interactive - インタラクティブモードでテスト実行"
	@echo "  byte-compile    - バイトコンパイル"
	@echo "  clean           - 生成ファイルの削除"
	@echo "  help            - このヘルプの表示"

test: ## バッチモードでテスト実行
	$(EMACS_CMD) \
		--load $(SOURCE_FILE) \
		--load $(TEST_FILE) \
		--funcall ert-run-tests-batch-and-exit

test-interactive: ## インタラクティブモードでテスト実行
	$(EMACS) --no-init-file --no-site-file \
		--load $(SOURCE_FILE) \
		--load $(TEST_FILE) \
		--eval "(ert-run-tests-interactively \"^editorconfig-ext-test-\")"

test-verbose: ## 詳細出力付きでテスト実行
	$(EMACS_CMD) \
		--eval "(setq ert-batch-backtrace-right-margin 1000)" \
		--load $(SOURCE_FILE) \
		--load $(TEST_FILE) \
		--funcall ert-run-tests-batch-and-exit

byte-compile: ## バイトコンパイル
	$(EMACS_CMD) \
		--load $(SOURCE_FILE) \
		--funcall batch-byte-compile \
		$(SOURCE_FILE)

clean: ## 生成ファイルの削除
	rm -vf *.elc
	rm -vf test/*.elc

# テスト用の一時ディレクトリクリーンアップ
test-clean: ## テスト実行時の一時ファイル削除
	rm -rf /tmp/editorconfig-ext-test*

# 全体的なクリーンアップ
clean-all: clean test-clean ## 全ての生成ファイルと一時ファイルを削除

# デバッグ用：設定確認
debug-config: ## 現在の設定を表示
	@echo "EMACS: $(EMACS)"
	@echo "EMACS_CMD: $(EMACS_CMD)"
	@echo "SOURCE_FILE: $(SOURCE_FILE)"
	@echo "TEST_FILE: $(TEST_FILE)"

# 環境テスト用：Emacsとload pathの確認
test-env: ## 環境とload pathの動作確認
	@echo "Testing Emacs environment..."
	$(EMACS_CMD) \
		--eval "(message \"Emacs version: %s\" emacs-version)" \
		--eval "(message \"Load path contains %d directories\" (length load-path))" \
		--eval "(dolist (dir load-path) (when (string-match \"editorconfig\" dir) (message \"EditorConfig path: %s\" dir)))" \
		--eval "(condition-case err (progn (require 'editorconfig) (message \"EditorConfig loaded successfully\")) (error (message \"EditorConfig load failed: %s\" err)))"
