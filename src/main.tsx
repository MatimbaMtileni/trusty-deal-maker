import { createRoot } from "react-dom/client";
import App from "./App.tsx";
import "./index.css";

const missingEnvVars: string[] = [];
const invalidEnvVars: string[] = [];

const supabaseUrl = import.meta.env.VITE_SUPABASE_URL;
const supabaseKey = import.meta.env.VITE_SUPABASE_PUBLISHABLE_KEY;

if (!supabaseUrl) {
  missingEnvVars.push("VITE_SUPABASE_URL");
} else {
  try {
    const parsed = new URL(supabaseUrl);
    if (!(parsed.protocol === "https:" || parsed.protocol === "http:")) {
      invalidEnvVars.push("VITE_SUPABASE_URL (must be a valid http/https URL)");
    }
  } catch {
    invalidEnvVars.push("VITE_SUPABASE_URL (must be a valid URL)");
  }
}

if (!supabaseKey) {
  missingEnvVars.push("VITE_SUPABASE_PUBLISHABLE_KEY");
}

const root = createRoot(document.getElementById("root")!);

if (missingEnvVars.length > 0 || invalidEnvVars.length > 0) {
  root.render(
    <div
      style={{
        fontFamily: "Inter, system-ui, sans-serif",
        padding: "2rem",
        lineHeight: 1.5,
      }}
    >
      <h1 style={{ fontSize: "1.25rem", fontWeight: 700, marginBottom: "0.75rem" }}>
        Invalid Supabase configuration
      </h1>

      {missingEnvVars.length > 0 && (
        <>
          <p style={{ marginBottom: "0.5rem" }}>
            Missing environment variable(s):
          </p>
          <code>{missingEnvVars.join(", ")}</code>
        </>
      )}

      {invalidEnvVars.length > 0 && (
        <>
          <p style={{ marginTop: "1rem", marginBottom: "0.5rem" }}>
            Invalid environment value(s):
          </p>
          <code>{invalidEnvVars.join(", ")}</code>
        </>
      )}

      <p style={{ marginTop: "1rem" }}>
        Update your <code>.env.local</code> and restart the dev server.
      </p>
    </div>
  );
} else {
  root.render(<App />);
}
