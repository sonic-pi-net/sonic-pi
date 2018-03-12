# Ruby/NArray

* ver 0.6.1.0 (2014-06-02)
* [Home page](http://masa16.github.io/narray/index.ja.html)
* [GitHub Repository](https://github.com/masa16/narray)
* [RubyGems](https://rubygems.org/gems/narray)
* [NArrayメソッド一覧](https://github.com/masa16/narray/blob/master/SPEC.ja.txt)

## Ruby/NArrayの特徴:

* Rubyで、高速な数値計算が可能。
* 要素には、8,16,32 bit 整数、単精度/倍精度の実数/複素数、
  および Rubyオブジェクトをサポート。
* 部分配列の取出し、代入も容易。
  要素位置の指定には、数値、範囲、インデックスの配列が使用可能。
* +, -, *, /, %, ** や 算術関数の演算は、要素-対-要素でおこなう。
* NMath 算術関数モジュール
* 配列同士の演算・代入は、各次元のサイズが同じであることが必要。
  ただし、サイズが1の次元は、他方の配列のサイズに合わせて
  「繰り返し」同じ要素が適用される。
* FFTW (高速フーリエ変換) version 3 は次のモジュールでサポート。
* Ruby/PGPLOT (グラフィックスライブラリ、別悃) にて
  XYグラフ、ヒストグラム、等高線、イメージ表示可能。
* 数値計算・画像処理・データ解析など幅広い応用が可能(と思う)。

* 類似品
  * Python/NumPy, Perl/PDL, Yorick, IDL

* 不十分な点
  * メソッドが不足。
  * バグ出しが不十分。
  * ドキュメントがない。

## インストール方法

Rubyの標準的な拡張ライブラリと同じです。ソースを展開したディレクトリで、

    ruby extconf.rb
    make
    make install

と実行します。

## 動作確認

* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* gcc version 4.4.7 20120313 (Red Hat 4.4.7-4) (GCC)

## 配布条件

Rubyのライセンスに準拠します。

## 著者

田中昌宏
