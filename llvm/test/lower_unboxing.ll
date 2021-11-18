; RUN: opt -load-pass-plugin=../librapid.so -passes=rapid-lower -S < %s | FileCheck %s
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128-ni:1"

%Object = type {
    i64 ; object header
  , [0 x i8*] ; payload
}

%ObjPtr = type %Object addrspace(1)*

declare fastcc noalias %ObjPtr @llvm.rapid.boxint(i64) "gc-leaf-function" readnone nounwind
declare fastcc i64 @llvm.rapid.unboxint(%ObjPtr noalias nocapture nofree) "gc-leaf-function" readnone nounwind
declare fastcc i1 @llvm.rapid.isdirect(%ObjPtr noalias nocapture nofree) "gc-leaf-function" readnone nounwind

define fastcc %ObjPtr @test_box(i64 %val) {
; CHECK: @test_box
; CHECK: shl i64
; CHECK-NEXT: or i64
; CHECK-NEXT: inttoptr
; CHECK-NEXT: addrspacecast
; CHECK-NOT: @llvm.rapid.boxint
; CHECK: ret
  %box = call fastcc %ObjPtr @llvm.rapid.boxint(i64 %val)
  ret %ObjPtr %box
}

define fastcc i64 @test_unbox(%ObjPtr %p) {
; CHECK: @test_unbox
; CHECK: addrspacecast
; CHECK-NEXT: ptrtoint
; CHECK-NEXT: ashr i64
; CHECK-NOT: @llvm.rapid.unboxint
; CHECK: ret
  %int = call fastcc i64 @llvm.rapid.unboxint(%ObjPtr %p)
  ret i64 %int
}

define fastcc i1 @test_isdirect(%ObjPtr %p) {
; CHECK: @test_isdirect
; CHECK: addrspacecast
; CHECK-NEXT: ptrtoint
; CHECK-NEXT: and i64
; CHECK-NEXT: icmp eq i64
; CHECK-NOT: @llvm.rapid.isdirect
; CHECK: ret
  %direct = call fastcc i1 @llvm.rapid.isdirect(%ObjPtr %p)
  ret i1 %direct
}
