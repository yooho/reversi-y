reversi-y
=========

・先手のときの最初の1手はランダム打ち(ロマン)<br>

Boardの評価は<br>
http://uguisu.skr.jp/othello/5-1.html<br>
2枚目の画像より<br><br>

・序盤：石の数15未満<br>
一つ打ってから、Boardの評価値+(-1)*相手の打てる場所の数 を最大化する<br>
・中盤：石の数15以上<br>
打てる場所が7なら深さ5、15以下なら深さ4、それ以上なら深さ3でBoardの評価値を最大化する。(Timeout対策)<br>
・終盤 ：石の数54以上<br>
終了時点での自分の石の数を最大化する探索<br><br>

探索はα-β法
