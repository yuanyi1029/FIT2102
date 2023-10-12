/** @type {import('vite').UserConfig} */
// https://vitejs.dev/config/

export default {
	server: {
		proxy: {
			'/api': {
				target: "http://localhost:3000/",
				changeOrigin: true
			}
		}
	}
}
