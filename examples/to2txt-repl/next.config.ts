import type { NextConfig } from "next";

const config: NextConfig = {
  webpack(config) {
    // Since Webpack 5 doesn't enable WebAssembly by default, we should do it manually
    config.experiments = { ...config.experiments, asyncWebAssembly: true };
    return config;
  },
};

export default config;
