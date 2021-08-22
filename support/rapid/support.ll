target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128-ni:1"

; generic boxed object
%Object = type {
    i64 ; object header
  , [0 x i8*] ; payload
}

%ObjPtr = type %Object addrspace(1)*
%ObjPtrPtr = type %Object addrspace(1)* addrspace(1)*
%RawPtr = type i8*
%RuntimePtr = type i8*
%FuncPtr = type i8*

%i8p1 = type i8 addrspace(1)*
%i32p1 = type i32 addrspace(1)*
%i64p1 = type i64 addrspace(1)*

%Idris_TSO.struct = type {
    i8* ; nurseryStart
  , i8* ; nurseryNext
  , i8* ; nurseryEnd
  , i32 ; errno
  , i8* ; stack_bottom
  , i8* ; stack_top
  , i8* ; stack_size
  , i64 ; heap_alloc
}

%TSOPtr = type %Idris_TSO.struct*

%Word = type i64

%VoidReturn = type {%RuntimePtr, %RuntimePtr, %RuntimePtr}
%Return1 = type {%RuntimePtr, %RuntimePtr, %ObjPtr}

%FuncPtrClosureEntry = type %Return1 (%RuntimePtr, %TSOPtr, %RuntimePtr, %ObjPtr, %ObjPtr)*

declare ccc void @idris_rts_gc(%TSOPtr, i8*)
declare ccc void @idris_rts_crash(i64) "gc-leaf-function" noreturn
declare ccc void @idris_rts_crash_msg(%ObjPtr) "gc-leaf-function" noreturn
declare ccc void @rapid_crash(i8*) "gc-leaf-function" noreturn
declare ccc void @idris_rts_crash_typecheck(%ObjPtr, i64) "gc-leaf-function" noreturn

declare ccc void @idris_mkcon_ok(%ObjPtr)
declare ccc void @idris_mkcon_arg_ok(%ObjPtr, i64)

declare ccc i64 @idris_rts_bits64_to_str(%i8p1 noalias nocapture nofree nonnull, i64) "gc-leaf-function" readonly argmemonly
declare ccc i64 @idris_rts_int_to_str(%i8p1 noalias nocapture nofree nonnull, i64) "gc-leaf-function" readonly argmemonly
declare ccc i64 @idris_rts_double_to_str(%i8p1 noalias nocapture nofree writeonly, i64, double) "gc-leaf-function" argmemonly
declare ccc double @idris_rts_str_to_double(%ObjPtr noalias nocapture nofree nonnull) "gc-leaf-function" readonly argmemonly
declare ccc i64 @idris_rts_str_to_int(%ObjPtr noalias nocapture nofree nonnull) "gc-leaf-function" readonly argmemonly

declare ccc void @rapid_strreverse(%i8p1 noalias nocapture nofree nonnull writeonly, %i8p1 noalias nocapture nofree nonnull readonly, i64) "gc-leaf-function" argmemonly

declare ccc void @rapid_system_exit(%TSOPtr, i64, %ObjPtr)
declare ccc i64 @rapid_system_errno(%TSOPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_strerror(%TSOPtr, i64, %ObjPtr)
declare ccc i64 @rapid_system_file_errno(%TSOPtr, %ObjPtr)
declare ccc i64 @rapid_system_system(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_fork(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_get_env(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @idris_rts_write_buffer_data(%TSOPtr, %ObjPtr, %ObjPtr, i64, i64, %ObjPtr)
declare ccc i64 @idris_rts_read_buffer_data(%TSOPtr, %ObjPtr, %ObjPtr, i64, i64, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_open(%TSOPtr, %ObjPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_fdopen(%TSOPtr, i64, %ObjPtr, %ObjPtr)
declare ccc void @rapid_system_file_close(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_flush(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_chmod(%TSOPtr, %ObjPtr, i64, %ObjPtr)
declare ccc %Word @rapid_system_file_eof(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %Word @rapid_system_file_remove(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_error(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_size(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_atime(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_ctime(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_mtime(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_stdin(%TSOPtr)
declare ccc %ObjPtr @rapid_system_file_stdout(%TSOPtr)
declare ccc %ObjPtr @rapid_system_file_stderr(%TSOPtr)
declare ccc %Word @rapid_system_file_write_string(%TSOPtr, %ObjPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_read_line(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_file_read_chars(%TSOPtr, i64, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_seek_line(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_current_dir(%TSOPtr, %ObjPtr)
declare ccc i64 @rapid_system_dir_create(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_dir_change(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_dir_open(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc void @rapid_system_dir_close(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_dir_next_entry(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_popen(%TSOPtr, %ObjPtr, %ObjPtr, %ObjPtr)
declare ccc void @rapid_system_pclose(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc void @rapid_system_free(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @rapid_system_file_read_char(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_stdin_getline(%TSOPtr, %ObjPtr)
declare ccc i64 @rapid_system_get_arg_count(%TSOPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_system_get_arg(%TSOPtr, i64, %ObjPtr)
declare ccc i64 @rapid_string_bytelength(%TSOPtr, %ObjPtr nocapture readonly) "gc-leaf-function"
declare ccc %ObjPtr @rapid_fast_pack(%TSOPtr, %ObjPtr)
declare ccc %ObjPtr @rapid_fast_append(%TSOPtr, %ObjPtr)
declare ccc void @rapid_putstr(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i32 @rapid_system_getchar(%TSOPtr, %ObjPtr)
declare ccc void @rapid_system_putchar(%TSOPtr, i32, %ObjPtr)
declare ccc i32 @rapid_clock_read(%TSOPtr, %ObjPtr, i32) "gc-leaf-function"

declare ccc %i8p1 @utf8_codepoints_to_bytes(%i8p1 nocapture readonly, i32, i32) "gc-leaf-function"
declare ccc i32 @utf8_codepoints_bytelen(%i8p1 nocapture readonly, i32, i32) "gc-leaf-function"
declare ccc i32 @utf8_decode1(%i8p1 nocapture readonly) "gc-leaf-function"
declare ccc i64 @utf8_decode1_length(%i8p1 nocapture readonly) "gc-leaf-function"
declare ccc i32 @utf8_encode1(%i8p1 nocapture writeonly, i32) "gc-leaf-function"
declare ccc i32 @utf8_bytes_to_codepoints(%i8p1 nocapture readonly, i32) "gc-leaf-function"

declare ccc i64 @idrnet_af_inet(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_af_inet6(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_af_unix(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_af_unspec(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_socket(%TSOPtr, i64, i64, i64, %ObjPtr) "gc-leaf-function"
declare ccc %ObjPtr @idrnet_create_sockaddr(%TSOPtr, %ObjPtr)
declare ccc void @idrnet_free(%TSOPtr, %ObjPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_bind(%TSOPtr, i64, i64, i64, %ObjPtr, i64, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_listen(%TSOPtr, i64, i64, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idrnet_accept(%TSOPtr, i64, %ObjPtr, %ObjPtr) "gc-leaf-function"

declare ccc i64 @idrnet_sockaddr_family(%TSOPtr, %ObjPtr, %ObjPtr) "gc-leaf-function"
declare ccc %ObjPtr @idrnet_sockaddr_ipv4(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc %ObjPtr @idrnet_sockaddr_unix(%TSOPtr, %ObjPtr, %ObjPtr)
declare ccc i64 @idrnet_sockaddr_ipv4_port(%TSOPtr, %ObjPtr, %ObjPtr) "gc-leaf-function"

; Terminal utility functions
declare ccc void @idris2_setupTerm(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idris2_getTermCols(%TSOPtr, %ObjPtr) "gc-leaf-function"
declare ccc i64 @idris2_getTermLines(%TSOPtr, %ObjPtr) "gc-leaf-function"

; GMP interface (partial)
%LimbPtr = type i64 addrspace(1)*
%LimbT = type i64
%mp_size_t = type i64
declare ccc %LimbT @__gmpn_add(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, i64, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc %LimbT @__gmpn_sub(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, i64, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc %LimbT @__gmpn_mul(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, i64, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc void @__gmpn_and_n(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc void @__gmpn_ior_n(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc void @__gmpn_xor_n(%LimbPtr nocapture writeonly, %LimbPtr nocapture readonly, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc %LimbT @__gmpn_lshift(%LimbPtr nocapture, %LimbPtr nocapture, i64, i32) "gc-leaf-function"
declare ccc %LimbT @__gmpn_rshift(%LimbPtr nocapture, %LimbPtr nocapture, i64, i32) "gc-leaf-function"
declare ccc void @__gmpn_tdiv_qr(%LimbPtr nocapture writeonly, %LimbPtr nocapture writeonly, %mp_size_t, %LimbPtr nocapture readonly, %mp_size_t, %LimbPtr nocapture readonly, %mp_size_t) "gc-leaf-function"
declare ccc i32 @__gmpn_cmp(%LimbPtr nocapture readonly, %LimbPtr nocapture readonly, i64) "gc-leaf-function"
declare ccc i64 @__gmpn_sizeinbase(%LimbPtr nocapture readonly, i64, i32) "gc-leaf-function"
declare ccc i64 @rapid_bigint_get_str(%ObjPtr nocapture, %ObjPtr nocapture readonly, i32) "gc-leaf-function"
declare ccc i32 @rapid_bigint_set_str(%ObjPtr writeonly noalias nocapture nofree, %ObjPtr readonly noalias nocapture nofree nonnull) nounwind argmemonly "gc-leaf-function"
declare ccc i64 @rapid_bigint_lshift_inplace(%LimbPtr nocapture, i64, i32) "gc-leaf-function"
declare ccc i64 @rapid_bigint_rshift_inplace(%LimbPtr nocapture, i64, i32) "gc-leaf-function"
declare ccc i64 @rapid_bigint_real_size(%LimbPtr nocapture readonly %p, i64 %n) "gc-leaf-function"

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture, i64, i1) nounwind
declare void @llvm.memcpy.p1i8.p1i8.i64(%i8p1 nocapture, %i8p1 nocapture, i64, i1) nounwind
declare void @llvm.memmove.p1i8.p1i8.i64(%i8p1 nocapture, %i8p1 nocapture, i64, i1) nounwind
declare void @llvm.memset.p1i8.i64(%i8p1 nocapture writeonly, i8, i64, i1) nounwind "gc-leaf-function"
declare void @llvm.dbg.addr(metadata, metadata, metadata)

; LLVM double intrinsics
declare double @llvm.ceil.f64(double) nounwind "gc-leaf-function"
declare double @llvm.cos.f64(double) nounwind "gc-leaf-function"
declare double @llvm.exp.f64(double) nounwind "gc-leaf-function"
declare double @llvm.floor.f64(double) nounwind "gc-leaf-function"
declare double @llvm.log.f64(double) nounwind "gc-leaf-function"
declare double @llvm.sin.f64(double) nounwind "gc-leaf-function"
declare double @llvm.sqrt.f64(double) nounwind "gc-leaf-function"

; from <math.h>
declare double @tan(double) nounwind "gc-leaf-function"
declare double @asin(double) nounwind "gc-leaf-function"
declare double @acos(double) nounwind "gc-leaf-function"
declare double @atan(double) nounwind "gc-leaf-function"

declare i8* @llvm.frameaddress(i32)
declare i8* @llvm.addressofreturnaddress()

declare ccc i1 @llvm.expect.i1(i1, i1) "gc-leaf-function"

; "abs" intrinsics not yet available
declare ccc i32 @llvm.abs.i32(i32, i1) "gc-leaf-function"
declare ccc i64 @llvm.abs.i64(i64, i1) "gc-leaf-function"
declare ccc i64 @llvm.ctlz.i64(i64, i1) "gc-leaf-function"

;can be removed when the intrinsic is available with LLVM 12+
define ccc i32 @rapid.abs.i32(i32 %arg0, i1 %arg1) "gc-leaf-function" {
  %1 = icmp slt i32 %arg0, 0
  br i1 %1, label %neg, label %pos
neg:
  %2 = sub i32 0, %arg0
  ret i32 %2
pos:
  ret i32 %arg0
}
define ccc i64 @rapid.abs.i64(i64 %arg0, i1 %arg1) "gc-leaf-function" {
  %1 = icmp slt i64 %arg0, 0
  br i1 %1, label %neg, label %pos
neg:
  %2 = sub i64 0, %arg0
  ret i64 %2
pos:
  ret i64 %arg0
}


declare %ObjPtr @llvm.ptrmask.p1obj.i64(%ObjPtr, i64)

; Custom Intrinsics
declare fastcc noalias %ObjPtr @llvm.rapid.boxint(i64) "gc-leaf-function" readnone nounwind
declare fastcc i64 @llvm.rapid.unboxint(%ObjPtr noalias nocapture nofree) "gc-leaf-function" readnone nounwind
declare fastcc i1 @llvm.rapid.isdirect(%ObjPtr noalias nocapture nofree) "gc-leaf-function" readnone nounwind

@error_msg_void = private unnamed_addr constant [23 x i8] c"Error: Executed 'void'\00"

define private fastcc %Return1 @rapid_gc_enter(%RuntimePtr %HpNewPtr, %TSOPtr %BaseArg, i64 %size.aligned) noinline gc "statepoint-example" {
  %frame = call i8* @llvm.addressofreturnaddress()

  %heapPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseArg, i32 0, i32 1
; this is only required for GC stats ("allocated bytes total" value):
; GC_STATS_START
  store %RuntimePtr %HpNewPtr, %RuntimePtr* %heapPtr
; GC_STATS_END

  ; store requested allocation size in base->heap_alloc
  %heapAllocPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseArg, i32 0, i32 7
  store i64 %size.aligned, i64* %heapAllocPtr

  call ccc void @idris_rts_gc(%TSOPtr %BaseArg, i8* %frame)

  ; get updated heap pointer from BaseTSO
  %heap = load %RuntimePtr, %RuntimePtr* %heapPtr

  %heapNext = getelementptr i8, %RuntimePtr %heap, i64 %size.aligned

  %newObject = addrspacecast %RuntimePtr %heap to %ObjPtr

  %packed1 = insertvalue %Return1 undef, %RuntimePtr %heapNext, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr undef, 1
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newObject, 2
  ret %Return1 %packed3
}

define private fastcc i1 @mem_eq(%i8p1 noalias nocapture nofree nonnull %v1, %i8p1 noalias nocapture nofree nonnull %v2, i64 %size) argmemonly readonly nounwind {
entry:
  %lengthZero = icmp eq i64 %size, 0
  br i1 %lengthZero, label %finished_eq, label %loop

loop:
  %i = phi i64 [%iPlus, %loopend], [0, %entry]

  %p1 = getelementptr inbounds i8, %i8p1 %v1, i64 %i
  %p2 = getelementptr inbounds i8, %i8p1 %v2, i64 %i
  %b1 = load i8, %i8p1 %p1
  %b2 = load i8, %i8p1 %p2
  %beq = icmp eq i8 %b1, %b2

  br i1 %beq, label %loopend, label %finished

loopend:
  %iPlus = add nuw nsw i64 %i, 1
  %continue = icmp ult i64 %iPlus, %size
  br i1 %continue, label %loop, label %finished
finished:
  ret i1 %beq

finished_eq:
  ret i1 1
}

define private fastcc i32 @rapid.memcmp(%i8p1 noalias nocapture nofree nonnull %v1, %i8p1 noalias nocapture nofree nonnull %v2, i64 %size) argmemonly readonly nounwind {
;define external fastcc i32 @rapid.memcmp(%i8p1 %v1, %i8p1 %v2, i64 %size) noinline optsize nounwind {
entry:
  %lengthZero = icmp eq i64 %size, 0
  br i1 %lengthZero, label %finished_eq, label %loop

loop:
  %i = phi i64 [%iPlus, %loopend], [0, %entry]

  %p1 = getelementptr inbounds i8, %i8p1 %v1, i64 %i
  %p2 = getelementptr inbounds i8, %i8p1 %v2, i64 %i
  %b1 = load i8, %i8p1 %p1
  %b2 = load i8, %i8p1 %p2
  %beq = icmp eq i8 %b1, %b2

  br i1 %beq, label %loopend, label %finished_neq

loopend:
  %iPlus = add nuw nsw i64 %i, 1
  %continue = icmp ult i64 %iPlus, %size
  br i1 %continue, label %loop, label %finished_eq
finished_neq:
  %bcmp = icmp ult i8 %b1, %b2
  %result = select i1 %bcmp, i32 -1, i32 1
  ret i32 %result

finished_eq:
  ret i32 0
}

define private fastcc i1 @rapid.ptrisnull(%ObjPtr noalias nocapture nofree readonly %p) "gc-leaf-function" readnone nounwind alwaysinline {
  %p.addrcasted = addrspacecast %Object addrspace(1)* %p to %Object addrspace(2)*
  %p.int = ptrtoint %Object addrspace(2)* %p.addrcasted to i64
  %isnull = icmp eq i64 0, %p.int
  ret i1 %isnull
}

declare ccc %ObjPtr @malloc(i64)

define external fastcc %Return1 @rapid_allocate_fast (%RuntimePtr %HpPtrArg, %TSOPtr %BaseArg, %RuntimePtr %HpLimPtrArg, i64 %size) noinline nounwind gc "statepoint-example" {
  %Hp = ptrtoint %RuntimePtr %HpPtrArg to i64

  %nurseryEndPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct *%BaseArg, i32 0, i32 2
  %nurseryEnd = load %RuntimePtr, %RuntimePtr* %nurseryEndPtr

  %HpLim = ptrtoint %RuntimePtr %nurseryEnd to i64

  %size.plus7 = add i64 %size, 7
  %size.aligned = and i64 -8, %size.plus7

  %HpNew = add i64 %Hp, %size.aligned
  %HpNewPtr = inttoptr i64 %HpNew to %RuntimePtr

  %overflow.in = icmp ugt i64 %HpNew, %HpLim

; ALWAYS FORCE GC:
  ;%overflow.in = icmp ugt i64 %HpNew, 0
  %overflow = call ccc i1 @llvm.expect.i1(i1 %overflow.in, i1 0)

  br i1 %overflow, label %gc_enter, label %continue
continue:
  %retptr = inttoptr i64 %Hp to i64*
  ;TODO: do it
  %packed1 = insertvalue %Return1 undef, %RuntimePtr %HpNewPtr, 0
  %packed2 = insertvalue %Return1 %packed1, %RuntimePtr %HpLimPtrArg, 1
  %newAddrIn1 = addrspacecast %RuntimePtr %HpPtrArg to i8 addrspace(1)*
  %newAddr = bitcast i8 addrspace(1)* %newAddrIn1 to %ObjPtr
  %packed3 = insertvalue %Return1 %packed2, %ObjPtr %newAddr, 2
  ret %Return1 %packed3
gc_enter:
  %gcresult = tail call fastcc %Return1 @rapid_gc_enter(%RuntimePtr %HpNewPtr, %TSOPtr %BaseArg, i64 %size.aligned)
  ret %Return1 %gcresult
}

define private fastcc i64 @idris_enter_stackbridge(%TSOPtr %BaseTSO, i8* %heapStart, i8* %heapEnd) {
  %result = call fastcc %Return1 @_$7b__mainExpression$3a0$7d(%RuntimePtr %heapStart, %TSOPtr %BaseTSO, %RuntimePtr %heapEnd)
  ;call hhvmcc %Return1 @Main$2e$7bmain$3a0$7d(%RuntimePtr %heapStart, %RuntimePtr %BaseTSO, %RuntimePtr %heapEnd, %ObjPtr undef)

; this is only required for GC stats at the end of the program:
; GC_STATS_START
  %HeapAfterExec = extractvalue %Return1 %result, 0
  %heapPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 1
  store %RuntimePtr %HeapAfterExec, %RuntimePtr* %heapPtr
; GC_STATS_END

  ret i64 0
}

define external ccc i64 @idris_enter(%Idris_TSO.struct* %BaseTSO) {
  %heapStartPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 1
  %heapStart = load %RuntimePtr, %RuntimePtr* %heapStartPtr

  %heapEndPtr = getelementptr inbounds %Idris_TSO.struct, %Idris_TSO.struct* %BaseTSO, i32 0, i32 2
  %heapEnd = load %RuntimePtr, %RuntimePtr* %heapEndPtr

  call fastcc i64 @idris_enter_stackbridge(%TSOPtr %BaseTSO, %RuntimePtr %heapStart, %RuntimePtr %heapEnd)
  ret i64 0
}

!llvm.module.flags = !{!3, !4}
!llvm.dbg.cu = !{!99}

!0 = !DIBasicType(name: "RuntimePtr", size: 8, align: 8, encoding: DW_ATE_address)
!1 = !DIBasicType(name: "ObjPtr", size: 8, align: 8, encoding: DW_ATE_address)
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}

!99 = distinct !DICompileUnit(language: DW_LANG_C99, file: !100)
!100 = !DIFile(directory: "/tmp", filename: "Main.idr")
