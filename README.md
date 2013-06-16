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

saykotoeri マイナーモードが有効な時には以下のコマンドが使用できます。

| コマンド  | 機能                     |
| --------- | ------------------------ |
| C-c ; f m | Modelファイルを開く      |
| C-c ; f c | Controllerファイルを開く |
| C-c ; f v | Viewファイルを開く       |

