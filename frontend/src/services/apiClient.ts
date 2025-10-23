import axios from 'axios';
import type { AxiosInstance, AxiosError, InternalAxiosRequestConfig, AxiosResponse } from 'axios';

/**
 * Base URL for the API.
 * Configured via environment variable or defaults to localhost:5555 (HTTP only in development).
 */
const BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:5555';

/**
 * Configured Axios instance for API communication.
 * Includes request/response interceptors for auth, error handling, and logging.
 */
const apiClient: AxiosInstance = axios.create({
  baseURL: BASE_URL,
  timeout: 30000, // 30 seconds timeout
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  withCredentials: true, // Enable CORS credentials
});

/**
 * Request interceptor to add authorization token and logging.
 */
apiClient.interceptors.request.use(
  (config: InternalAxiosRequestConfig) => {
    // Add authorization token if available
    const token = localStorage.getItem('authToken');
    if (token && config.headers) {
      config.headers.Authorization = `Bearer ${token}`;
    }

    // Log request in development
    if (import.meta.env.DEV) {
      console.log(`[API Request] ${config.method?.toUpperCase()} ${config.url}`, config.data);
    }

    return config;
  },
  (error: AxiosError) => {
    console.error('[API Request Error]', error);
    return Promise.reject(error);
  }
);

/**
 * Response interceptor for error handling and logging.
 */
apiClient.interceptors.response.use(
  (response: AxiosResponse) => {
    // Log response in development
    if (import.meta.env.DEV) {
      console.log(`[API Response] ${response.config.method?.toUpperCase()} ${response.config.url}`, response.data);
    }

    return response;
  },
  (error: AxiosError<ErrorResponse>) => {
    // Handle common HTTP errors
    if (error.response) {
      const { status, data } = error.response;

      // Log error details
      console.error(`[API Error] ${status}:`, data);

      // Handle specific status codes
      switch (status) {
        case 401:
          // Unauthorized - clear token and redirect to login
          localStorage.removeItem('authToken');
          window.location.href = '/login';
          break;

        case 403:
          // Forbidden - show access denied message
          console.error('Acesso negado:', data.message);
          break;

        case 404:
          // Not found
          console.error('Recurso não encontrado:', data.message);
          break;

        case 500:
          // Internal server error
          console.error('Erro interno do servidor:', data.message);
          break;

        default:
          console.error('Erro na requisição:', data.message);
      }

      // Return structured error response
      return Promise.reject({
        status,
        message: data.message || 'Erro ao processar requisição',
        details: data.details,
        traceId: data.traceId,
        validationErrors: data.validationErrors,
      });
    } else if (error.request) {
      // Request was made but no response received
      console.error('[API No Response]', error.request);
      return Promise.reject({
        status: 0,
        message: 'Sem resposta do servidor. Verifique sua conexão.',
      });
    } else {
      // Something happened in setting up the request
      console.error('[API Setup Error]', error.message);
      return Promise.reject({
        status: 0,
        message: error.message || 'Erro ao configurar requisição',
      });
    }
  }
);

/**
 * Error response structure matching backend ErrorResponse DTO.
 */
export interface ErrorResponse {
  statusCode: number;
  message: string;
  details?: string;
  traceId?: string;
  timestamp: string;
  path?: string;
  validationErrors?: Record<string, string[]>;
  errorCode?: string;
}

/**
 * Structured API error for client-side error handling.
 */
export interface ApiError {
  status: number;
  message: string;
  details?: string;
  traceId?: string;
  validationErrors?: Record<string, string[]>;
}

export default apiClient;
