self: super: {
  diagnostica = self.callPackage ./diagnostica {};
  diagnostica-sage = self.callPackage ./diagnostica-sage {};
  generics-eot = self.callPackage ./generics-eot {};
  sage = self.callPackage ./sage {};
  sage-parsers-instances = self.callPackage ./sage-parsers-instances {};
}
