import { ReactNode } from "react";
import type { Metadata, Viewport } from "next";

import "./globals.css";

export const metadata: Metadata = {
  title: "to2txt repl",
  description: "A repl for the to2txt parser.",
};

export const viewport: Viewport = {
  width: "device-width",
  initialScale: 1,
  maximumScale: 1,
  userScalable: false,
};

export interface RootLayoutProps {
  children?: ReactNode;
}

export default function RootLayout(props: RootLayoutProps) {
  return (
    <html lang="en">
      <body>
        <div className="absolute top-0 right-0 bottom-0 left-0 viewport">
          {props.children}
        </div>
      </body>
    </html>
  );
}
