11 Minecraft Pi

# Minecraft Pi（マインクラフトパイ）
Sonic Piは現在、 Minecraft Piと対話するためのシンプルなAPIをサポートしています。Minecraftの特別版は、Raspberry PiのLinuxベースのオペレーティングシステムRaspbianにデフォルトでインストールされています。

## ライブラリは不必要

Minecraft Piは、とっても簡単に扱えるよう設計されています。必要なことは、Minecraft Piを起動して世界を創造するだけです。その後、`play` や `synth` を扱うように `mc_*`関数を自由に使います。何かのライブラリをインストールしたり、インポートする必要はありません。箱から出して動かしてみましょう。

## 自動接続
The Minecraft Pi API はMinecraft Piアプリケーションへの接続を可能にします。あなたは何も心配をしなくてもよいということです。 Minecraft Piを起動せずに、Minecraft Pi APIを使おうとした場合には、Sonic Piはこれを丁寧に教えてくれます。同様に、`live_loop`を実行する一方で、もしも、MinecraftPiを閉じてしまっても、live_loopの接続を停止し、接続できてないことを丁寧に伝えてくれます。再接続するために、再びMinecraft Piを起動して、Sonic Piが自動検出して、再接続を試みます。

## ライブコーディングのデザイン

Minecraft Pi APIは`live_loop`内でシームレスに動作するように設計されています。
これは、SonicPiの音に変更を加え、Minecraft Piの世界の変更と同期させることが可能であることを意味します。
インスタントなMinecraftベースのミュージックビデオです！ Minecraft Piはアルファ版のソフトウェアであり、
わずかに不安定であることに注意してください。何か問題が発生した場合は、単純にMinecraft Piを再起動し、以前と同様に続けましょう。Sonic Piの自動接続機能が対応します。

## Raspberry Pi 2.0が必要

Sonic　PiとMinecraftの両方を実行したい場合は、特にSonic　Piのサウンド機能を使用したい場合は
Raspberry Pi 2.0を使用することをお勧めします。

## APIサポート

現段階では、Sonic　Piは、次のセクション11.1に詳述されている基本ブロックとプレイヤーの操作をサポートしています。
世界の中のプレイヤーの相互作用によるトリガーされるイベントのコールバックのサポートは、
将来のリリースバージョンで予定されています。

