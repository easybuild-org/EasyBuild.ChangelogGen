module Workspace

open EasyBuild.FileSystemProvider

type Workspace = AbsoluteFileSystem<"./fixtures">
