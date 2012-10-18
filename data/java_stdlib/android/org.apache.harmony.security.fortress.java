package org.apache.harmony.security.fortress;
class Services {
  int providersNames;
  int providers;
  int refreshNumber;
  int needRefresh;
  int secureRandom;
  int services;
}
class SecurityAccess {
}
class Engine {
  class SpiAndProvider {
    int provider;
    int spi;
  }
  class ServiceCacheEntry {
    int service;
    int refreshNumber;
    int algorithm;
  }
  int serviceCache;
  int serviceName;
  int door;
}
