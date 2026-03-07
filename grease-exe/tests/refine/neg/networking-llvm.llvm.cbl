; Copyright (c) Galois, Inc. 2025

; Test that the overrides for networking-related functions work as expected
; in the LLVM backend. This is analogous to tests/refine/neg/networking/test.c,
; but uses S-expressions instead of machine code.

(declare @abort () Unit)

; htons(5000) = 34835 in network byte order
(defun @test () Unit
  (start start:
    ; socket(AF_INET=2, SOCK_STREAM=1, 0)
    (let af_inet (ptr 64 0 (bv 64 2)))
    (let sock_stream (ptr 64 0 (bv 64 1)))
    (let zero64 (ptr 64 0 (bv 64 0)))
    (let sock_fd (funcall @socket af_inet sock_stream zero64))
    (let sock_fd_off (ptr-offset 64 sock_fd))
    ; Check if socket returned -1 (error)
    (let sock_err (equal? sock_fd_off (bv 64 18446744073709551615)))
    (branch sock_err done: bind:))

  (defblock bind:
    ; Allocate a sockaddr_in struct on the stack:
    ;   struct sockaddr_in {
    ;     sa_family_t sin_family;  // 2 bytes, AF_INET = 2
    ;     in_port_t   sin_port;   // 2 bytes, htons(5000) = 34835 = 0x8813
    ;     ...                     // remaining fields (12 bytes)
    ;   }
    ; Total: 16 bytes
    (let addr (alloca none (bv 64 16)))
    ; Write sin_family = 2 (AF_INET) as 2-byte value
    (let family_val (ptr 16 0 (bv 16 2)))
    (store none i16 addr family_val)
    ; Write sin_port = htons(5000) = 34835 at offset 2
    (let port_ptr (ptr-add-offset addr (bv 64 2)))
    (let port_val (ptr 16 0 (bv 16 34835)))
    (store none i16 port_ptr port_val)

    ; bind(sock_fd, addr, sizeof(sockaddr_in)=16)
    (let sixteen (ptr 64 0 (bv 64 16)))
    (let bind_rc (funcall @bind sock_fd addr sixteen))
    (let bind_off (ptr-offset 64 bind_rc))
    (let bind_err (equal? bind_off (bv 64 18446744073709551615)))
    (branch bind_err done: listen:))

  (defblock listen:
    ; listen(sock_fd, 5)
    (let five (ptr 64 0 (bv 64 5)))
    (let listen_rc (funcall @listen sock_fd five))
    (let listen_off (ptr-offset 64 listen_rc))
    (let listen_err (equal? listen_off (bv 64 18446744073709551615)))
    (branch listen_err done: accept:))

  (defblock accept:
    ; accept(sock_fd, NULL, NULL)
    (let null_ptr (ptr 64 0 (bv 64 0)))
    (let conn_fd (funcall @accept sock_fd null_ptr null_ptr))
    (let conn_off (ptr-offset 64 conn_fd))
    (let conn_err (equal? conn_off (bv 64 18446744073709551615)))
    (branch conn_err done: recv:))

  (defblock recv:
    ; Allocate a 4-byte buffer
    (let buf (alloca none (bv 64 4)))
    ; recv(conn_fd, buf, 4, 0)
    (let four (ptr 64 0 (bv 64 4)))
    (let zero_flags (ptr 64 0 (bv 64 0)))
    (let recv_rc (funcall @recv conn_fd buf four zero_flags))
    (let recv_off (ptr-offset 64 recv_rc))
    (let recv_err (equal? recv_off (bv 64 18446744073709551615)))
    (branch recv_err done: check-buf:))

  (defblock check-buf:
    ; Check if buf == "ABCD"
    (let b0 (load none i8 buf))
    (let buf1 (ptr-add-offset buf (bv 64 1)))
    (let b1 (load none i8 buf1))
    (let buf2 (ptr-add-offset buf (bv 64 2)))
    (let b2 (load none i8 buf2))
    (let buf3 (ptr-add-offset buf (bv 64 3)))
    (let b3 (load none i8 buf3))
    (let b0_off (ptr-offset 8 b0))
    (let b1_off (ptr-offset 8 b1))
    (let b2_off (ptr-offset 8 b2))
    (let b3_off (ptr-offset 8 b3))
    (let is_A (equal? b0_off (bv 8 65)))
    (let is_B (equal? b1_off (bv 8 66)))
    (let is_C (equal? b2_off (bv 8 67)))
    (let is_D (equal? b3_off (bv 8 68)))
    (let is_AB (and is_A is_B))
    (let is_CD (and is_C is_D))
    (let is_ABCD (and is_AB is_CD))
    (branch is_ABCD abort: send:))

  (defblock abort:
    (funcall @abort)
    (return ()))

  (defblock send:
    ; send(conn_fd, buf, 4, 0)
    (let send_four (ptr 64 0 (bv 64 4)))
    (let send_zero (ptr 64 0 (bv 64 0)))
    (let send_rc (funcall @send conn_fd buf send_four send_zero))
    (jump done:))

  (defblock done:
    (return ())))

(declare @socket ((domain (Ptr 64)) (type (Ptr 64)) (protocol (Ptr 64))) (Ptr 64))
(declare @bind ((sockfd (Ptr 64)) (addr (Ptr 64)) (addrlen (Ptr 64))) (Ptr 64))
(declare @listen ((sockfd (Ptr 64)) (backlog (Ptr 64))) (Ptr 64))
(declare @accept ((sockfd (Ptr 64)) (addr (Ptr 64)) (addrlen (Ptr 64))) (Ptr 64))
(declare @recv ((sockfd (Ptr 64)) (buf (Ptr 64)) (len (Ptr 64)) (flags (Ptr 64))) (Ptr 64))
(declare @send ((sockfd (Ptr 64)) (buf (Ptr 64)) (len (Ptr 64)) (flags (Ptr 64))) (Ptr 64))

;; flags {"--symbol", "test"}
;; flags {"--sym-file", "0:/network/AF_INET/SOCK_STREAM/socket"}
;; flags {"--sym-file", "4:/network/AF_INET/SOCK_STREAM/34835/0"}
;; go(prog)
;; could_not_infer()
;; check "Call to abort"
;; check [[
;; Concretized filesystem:
;;   /network/AF_INET/SOCK_STREAM/34835/0
;;     41 42 43 44
;; ]]
