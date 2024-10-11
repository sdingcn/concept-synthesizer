set(LLVM_LINK_COMPONENTS support)

add_clang_executable(concept-synthesizer
  ConceptSynthesizer.cpp
  )
target_link_libraries(concept-synthesizer
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )
