import React, { JSX, useEffect, useState } from "react";
import clsx from "clsx";

export interface OutputProps {
  className?: string;
  value?: string;
}

export default function Output(props: OutputProps): JSX.Element {
  const [parser, setParser] = useState<{ parse(input: string): string }>();
  const [error, setError] = useState();

  useEffect(() => {
    import("../../../wasm/pkg").then(setParser, setError);
  }, []);

  if (error) {
    throw error;
  }

  return (
    <div className={clsx("text-white/50 whitespace-pre-wrap", props.className)}>
      {parser && props.value && parser.parse(props.value)}
    </div>
  );
}
