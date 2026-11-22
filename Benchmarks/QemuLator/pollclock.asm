; Temporary resident QDOS 50 Hz counter, used only in the isolated timing VM.
; Install once with CALL after loading into at least 80 bytes of RESPR storage.
; The external parent runner reads the counter long at offset 36.
; MT.LPOLL takes an absolute link address; the following long is the callback address.
; The callback receives D3 = number of pending polls, including deferred polls.
; Byte encoding is retained in pollclock.hex; no ROM is modified.

init:       movem.l d0-d7/a0-a6,-(sp)
            lea     linkage(pc),a0
            lea     poll(pc),a1
            move.l  a1,4(a0)
            moveq   #$1c,d0
            trap    #1
            movem.l (sp)+,d0-d7/a0-a6
            moveq   #0,d0
            rts
linkage:    dc.l    0,0
counter:    dc.l    0
poll:       move.l  a0,-(sp)
            move.l  d0,-(sp)
            moveq   #0,d0
            move.w  d3,d0
            lea     counter(pc),a0
            add.l   d0,(a0)
            move.l  (sp)+,d0
            movea.l (sp)+,a0
            rts
