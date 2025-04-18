;; -*- compile-command: "cd .. && jsane -i src -o public" -*-
(js
 (when-ready
  (λ ()
    (define *png-header*
      (new 'Uint8Array '(#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)))

    (define lst (document.getElementById "chunk-list"))
    (define img (document.getElementById "img"))
    (define hex (document.getElementById "hexed"))
    (define textual (document.getElementById "textual"))
    (define result (document.getElementById "result"))
    (define result-data (document.getElementById "result-data"))
    (define trybtn (document.getElementById "trybtn"))

    (define (as-hex n)
      (let ((s ((ref n 'toString) 16)))
        (+ "0x"
           (if (eq? (len s) 1)
               (+ "0" s)
               s))))

    (define (u8arr->b64 buff type cont)
      (let ((rdr (new 'FileReader)))
        (set! rdr.onload (λ (_) (cont rdr.result)))
        (rdr.readAsDataURL (new 'File (list buff) "" (object (type => type))))))

    (define (chunkize l size join-string)
      (let ((vs (list)))
        (while (> (len l) 0)
          (vs.push ((ref (l.splice 0 size) 'join) join-string)))
        vs))

    (define (xor-data data xors)
      (print "xors: " xors)
      (let ((i 0))
        (map (λ (x)
               (let ((v (bxor x (list-ref xors (modulo i (len xors))))))
                 (set! i (+ i 1))
                 v))
             data)))

    (define (show-xorred! data)
      (let ((xors (map (λ (x) (parseInt x)) (xor.value.split " ")))
            (xorred-data (xor-data data xors))
            (u8arr (new 'Uint8Array xorred-data))
            (arr (ref u8arr 'buffer)))
        (set!
         result-data.innerText
         ((ref (chunkize (take (spread ((ref (new 'TextDecoder) 'decode) arr)) 1024) 10 "") 'join) "\n"))
        (set! trybtn.onclick
              (λ ()
                (print "We fuckin clicked")
                (u8arr->b64 u8arr "image/png" (λ (uri) (set! result.src uri)))))))

    (define (make-chunk-el! type data)
      (let ((fset   (element! "fieldset"))
            (legend (element! "legend"))
            (span   (element! "span"))
            (btn    (element! "button"))
            (type   (String.fromCharCode (spreading type))))
        (set! legend.innerText type)
        (set! span.innerText (+ (+ "length: " (len data)) " "))
        (set! btn.innerText "show bytes")
        (let ((arr (ref (new 'Uint8Array (spread data)) 'buffer)))
          (btn.addEventListener
           "click"
           (λ (_)
             (set!
              textual.innerText
              ((ref (chunkize (take (spread ((ref (new 'TextDecoder) 'decode) arr)) 1024) 10 "") 'join) "\n"))
             (set!
              hexed.innerText
              ((ref (chunkize (take (map as-hex data) 1024) 10 " ") 'join) "\n"))
             (show-xorred! data)
             )))

        (fset.appendChild legend)
        (fset.appendChild span)
        (fset.appendChild btn)

        (lst.appendChild fset)))

    (define (u8vec=? a b)
      (equal?
       (JSON.stringify (Array.from a))
       (JSON.stringify (Array.from b))))

    (define (->> ob fun)
      ((ref ob 'then) fun))

    (define (err why)
      (console.error why))

    (define (bytes->n l)
      (fold (λ (a b) (bior (<< a 8) b)) 0 l))

    (define (parse-chunks arr cb)
      (while (not (null? arr))
        (let ((len  (bytes->n (take arr 4)))
              (type (take (drop arr 4) 4))
              (data (take (drop arr 8) len))
              (crc  (take (drop arr (+ 8 len)) 4)))
          (cb type data)
          ((ref arr 'splice) 0 (+ 12 len))))
      true)

    (define (parse-png data)
      (if (not (u8vec=? (data.slice 0 (len *png-header*)) *png-header*))
          (begin
            (print (data.slice 0 (len *png-header*)))
            (print *png-header*)
            (err "This does not seem to be a png file"))
          (parse-chunks
           ((ref (Array.from data) 'splice) (len *png-header*))
           make-chunk-el!)))

    (document.addEventListener
     "drop"
     (λ (e)
       (e.preventDefault)
       (map
        (λ (item)
          (when (eqv? item.kind "file")
            (let ((file (item.getAsFile)))
              (->> (file.bytes)
                   (λ (u8arr)
                     (u8arr->b64
                      u8arr
                      "image/png"
                      (λ (uri)
                        (set! img.src uri)))
                     (parse-png u8arr))))))
        (spread e.dataTransfer.items))))

    (document.addEventListener
     "dragover"
     (λ (e)
       (e.preventDefault))))
  )))
