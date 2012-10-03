// see http://dsmweb.org/dsmweb/en/understand-dsm/technical-dsm-tutorial/partitioning.html

void a() { 
  c();
}

void b() { 
  c();
  d();
}

void c() { 
  a();
  e();
}

void d() { 
  a();
  g();
}

void e() { 
}

void f() { 
  c();
  e();
  g();
}

void g() { 
  b();
  c();
}
