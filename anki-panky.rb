class AnkiPanky < Formula
  desc "Pandoc Markdown Anki collection compiler for generating Anki collections from Pandoc-style Markdown files"
  homepage "https://github.com/kamalsacranie/anki-panky"
  url ""
  if Hardware::CPU.arm?
    url "https://example.com/package-arm.tar.gz"
    sha256 "..."
  else
    url "https://github.com/kamalsacranie/anki-panky/releases/download/0.0.0.1/anki-panky-0.0.0.1-x86_64-macos.tar.gz"
    sha256 "725be26e5eab0c3bf417cb368717e82ddd11cd422a161307163541bb4fc3029f"
  end
  license "GPL-3.0-or-later"

  def install
    prefix.install Dir["*"]
    bin.install_symlink "#{prefix}/anki-panky"
  end

  test do
    assert_match "0.0.0.1", shell_output("#{bin}/anki-panky --version")
  end
end
