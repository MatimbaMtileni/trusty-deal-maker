import { createRoot } from "react-dom/client";
import App from "./App.tsx";
import "./index.css";

const missingEnvVars: string[] = [];

if (!import.meta.env.VITE_SUPABASE_URL) {
  missingEnvVars.push("VITE_SUPABASE_URL");
}

if (!import.meta.env.VITE_SUPABASE_PUBLISHABLE_KEY) {
  missingEnvVars.push("VITE_SUPABASE_PUBLISHABLE_KEY");
}

const root = createRoot(document.getElementById("root")!);

if (missingEnvVars.length > 0) {
  root.render(
    <div
      style={{
        fontFamily: "Inter, system-ui, sans-serif",
        padding: "2rem",
        lineHeight: 1.5,
      }}
    >
      <h1 style={{ fontSize: "1.25rem", fontWeight: 700, marginBottom: "0.75rem" }}>
        Missing Supabase configuration
      </h1>
      <p style={{ marginBottom: "0.5rem" }}>
        Set the following environment variable(s) and restart the app:
      </p>
      <code>{missingEnvVars.join(", ")}</code>
    </div>
  );
} else {
  root.render(<App />);
}
