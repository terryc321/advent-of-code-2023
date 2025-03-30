;; guile
;;(getcwd)
;;(chdir "day20/guile")


(define broadcaster '( sr  gd  mg  hf))

(define conjunctions 
  '((ps   xc  mq  jt  zs  sr  nt  pq)
    (xc   zh)
    (ml   bp  gd  qv  kq)
    (th   zh)
    (zh   rx)
    (pd   zh)
    (kh   jg  qx  md  th  hf  dm  kk)
    (mk   kf  qn  nh  pd  dq  mg  bm)
    (bp   zh)))

(define flipflops
  '((jb   ps)
    (cm   ps  tm)
    (sl   ml  cp)
    (qr   ml)
    (hf   kh  jg)
    (jg   kk)
    (jt   pq)
    (qv   kv)
    (rj   mm  kh)
    (kf   xt)
    (kx   vk  mk)
    (dq   qn)
    (jk   hh  ps)
    (rr   mk  nh)
    (hs   kh  mb)
    (mg   mk  kf)
    (xt   dq  mk)
    (mq   nt)
    (nh   bm)
    (md   hs)
    (vk   mk  vl)
    (mm   kh)
    (kc   ps  jk)
    (kk   dm)
    (jn   ll  ml)
    (pp   kh  md)
    (zf   ml  bd)
    (qx   pp)
    (qn   rr)
    (mb   qb  kh)
    (nt   jt)
    (vl   zk  mk)
    (gd   ml  rm)
    (hh   ps  jb)
    (tm   ps  mq)
    (kv   jn  ml)
    (zs   kc)
    (ll   ml  kq)
    (cp   qv  ml)
    (rm   sl  ml)
    (bd   qr  ml)
    (dm   qx)
    (qb   rj  kh)
    (pq   zs)
    (bm   kx)
    (sr   cm  ps)
    (zk   mk)
    (kq   zf)))

;; each symbol either conjunction OR flipflop not both , can be in broadcaster also
;; how many symbols are there ?

;; (pulse SYM lo) low pulse
;; (pulse SYM hi) hi pulse 

