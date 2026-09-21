// Google Apps Script para setup.html
// 1. Ve a https://script.google.com/ y crea un proyecto nuevo.
// 2. Sustituye el contenido del editor por este archivo.
// 3. Cambia RECIPIENT si quieres recibir los avisos en otra dirección.
// 4. Implementar > Nueva implementación > Aplicación web.
//    Ejecutar como: Yo | Quién tiene acceso: Cualquiera.
// 5. Copia la URL que termina en /exec y pégala en APPS_SCRIPT_URL de setup.html.

const RECIPIENT = "jazelouled@gmail.com";

function doPost(e) {
  try {
    const data = JSON.parse(e.postData.contents);
    const participant = String(data.name || "").trim();

    if (!participant) {
      return jsonResponse({ ok: false, error: "Falta el nombre de la persona participante." });
    }

    MailApp.sendEmail({
      to: RECIPIENT,
      subject: "Confirmación de preparación · Taller SDM Málaga 2026",
      body:
        "Hola Jazel,\n\n" +
        participant + " ha completado la preparación previa del taller.\n\n" +
        "Taller: " + (data.workshop || "Taller SDM Málaga 2026") + "\n" +
        "Fecha de confirmación: " + new Date().toLocaleString("es-ES", { timeZone: "Europe/Madrid" }) + "\n"
    });

    return jsonResponse({ ok: true });
  } catch (error) {
    return jsonResponse({ ok: false, error: error.message });
  }
}

function doGet() {
  return ContentService
    .createTextOutput("Taller SDM Málaga 2026 · endpoint de confirmación activo")
    .setMimeType(ContentService.MimeType.TEXT);
}

function jsonResponse(data) {
  return ContentService
    .createTextOutput(JSON.stringify(data))
    .setMimeType(ContentService.MimeType.JSON);
}
