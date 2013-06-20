概要
======================
FuelPHP の開発をサポートするためのマイナーモード


インストール
------------
fuelphp-mode.el をload-pathが通っているところに置いて

    (require 'fuelphp-mode)

使い方
------
FuelPHPプロジェクト内のファイルをfind-fileで開くと自動的に有効になります。
また、FuelPHPプロジェクト内のファイルを開いている時に

    M-x fuelphp-mode

を実行することによりよっても有効になります。

FuelPHPマイナーモードが有効な時には以下のコマンドが使用できます。

| コマンド  | 機能                     |
| --------- | ------------------------ |
| C-c . f m | Modelファイルを開く      |
| C-c . f c | Controllerファイルを開く |
| C-c . f v | Viewファイルを開く       |
| C-c . o s | サーバを起動する         |
| C-c . o c | コンソールを起動する     |
| C-c . g   | fuel/app内をgrepする     |

カスタマイズ変数
---------------

### fuelphp-tags-file-name

FuelPHPプロジェクト内で利用しているタグファイルの名前。
プロジェクトROOT直下にこの変数で指定された名前のタグファイルがあるものとして検索されます。

デフォルト値: TAGS


ライセンス
----------
&copy; 2013  Mugijiru. This project is licensed under the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. license. See LICENSE for details.
