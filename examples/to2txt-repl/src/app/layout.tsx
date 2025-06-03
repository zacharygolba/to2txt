import { ReactNode } from "react";
import type { Metadata } from "next";

import "./globals.css";

export const metadata: Metadata = {
  title: "to2txt repl",
  description: "A repl for the to2txt parser.",
};

export interface RootLayoutProps {
  children?: ReactNode;
}

export default function RootLayout(props: RootLayoutProps) {
  return (
    <html lang="en">
      <body>{props.children}</body>
    </html>
  );
}
