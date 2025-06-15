-- MASTER QUERY: The Lifeblood of Network Protocol Analysis
-- This query demonstrates the complete data flow through all analysis layers
-- From raw packets → transport → encoding → content → security analysis

WITH packet_flow AS (
  -- Core packet flow with timing analysis
  SELECT 
    s.name AS session_name,
    p.frame_number,
    datetime(p.timestamp, 'unixepoch') AS packet_time,
    p.packet_size,
    
    -- Transport layer details
    t.protocol AS transport_protocol,
    t.src_ip || ':' || COALESCE(t.src_port, 0) AS source_endpoint,
    t.dst_ip || ':' || COALESCE(t.dst_port, 0) AS dest_endpoint,
    t.tcp_flags,
    t.payload_size,
    
    -- Encoding layer analysis
    e.encoding_type AS application_protocol,
    e.protocol_version,
    e.compression_type,
    e.encryption_detected,
    e.body_size,
    
    -- Content analysis
    c.content_type,
    c.content_hash,
    CASE 
      WHEN c.decoded_content IS NOT NULL THEN 
        substr(c.decoded_content, 1, 100) || 
        CASE WHEN length(c.decoded_content) > 100 THEN '...' ELSE '' END
      ELSE '[Binary/Encrypted]'
    END AS content_preview,
    c.vulnerabilities_detected,
    c.suspicious_patterns,
    
    -- Row identifiers for joins
    p.packet_id,
    t.transport_id,
    e.encoding_id,
    c.content_id
    
  FROM sessions s
  JOIN packets p ON s.session_id = p.session_id
  JOIN transport_layer t ON p.packet_id = t.packet_id
  LEFT JOIN encoding_layer e ON t.transport_id = e.transport_id
  LEFT JOIN content_layer c ON e.encoding_id = c.encoding_id
),

security_analysis AS (
  -- Security alerts and rule matches
  SELECT 
    pf.packet_id,
    pf.transport_id,
    pf.encoding_id,
    pf.content_id,
    GROUP_CONCAT(
      ar.name || ' (' || rm.severity || '): ' || 
      substr(rm.matched_data, 1, 50) || 
      CASE WHEN length(rm.matched_data) > 50 THEN '...' ELSE '' END,
      ' | '
    ) AS security_alerts,
    MAX(
      CASE rm.severity 
        WHEN 'CRITICAL' THEN 4
        WHEN 'HIGH' THEN 3
        WHEN 'MEDIUM' THEN 2
        WHEN 'LOW' THEN 1
        ELSE 0
      END
    ) AS max_severity_level,
    COUNT(rm.match_id) AS alert_count
  FROM packet_flow pf
  LEFT JOIN rule_matches rm ON (
    pf.packet_id = rm.packet_id OR
    pf.transport_id = rm.transport_id OR
    pf.encoding_id = rm.encoding_id OR
    pf.content_id = rm.content_id
  )
  LEFT JOIN analysis_rules ar ON rm.rule_id = ar.rule_id
  GROUP BY pf.packet_id, pf.transport_id, pf.encoding_id, pf.content_id
),

protocol_specific AS (
  -- Protocol-specific analysis (HTTP, TLS, DNS, etc.)
  SELECT 
    pf.encoding_id,
    
    -- HTTP analysis
    CASE WHEN h.http_id IS NOT NULL THEN
      json_object(
        'method', h.method,
        'uri', h.uri,
        'status_code', h.status_code,
        'user_agent', substr(h.user_agent, 1, 50),
        'response_time', h.response_time,
        'sql_injection_risk', h.sql_injection_risk,
        'xss_risk', h.xss_risk,
        'path_traversal_risk', h.path_traversal_risk
      )
    END AS http_details,
    
    -- TLS analysis
    CASE WHEN tls.tls_id IS NOT NULL THEN
      json_object(
        'version', tls.version,
        'cipher_suite', tls.cipher_suite,
        'server_name', tls.server_name,
        'handshake_complete', tls.handshake_complete,
        'weak_cipher', tls.weak_cipher,
        'cert_issues', tls.cert_issues
      )
    END AS tls_details,
    
    -- File extraction
    CASE WHEN ef.file_id IS NOT NULL THEN
      json_object(
        'filename', ef.filename,
        'file_type', ef.file_type,
        'file_size', ef.file_size,
        'malware_detected', ef.malware_detected
      )
    END AS extracted_file_details
    
  FROM packet_flow pf
  LEFT JOIN http_analysis h ON pf.encoding_id = h.encoding_id
  LEFT JOIN tls_analysis tls ON pf.encoding_id = tls.encoding_id
  LEFT JOIN extracted_files ef ON pf.content_id = ef.content_id
),

flow_context AS (
  -- Flow-level context and statistics
  SELECT 
    pf.source_endpoint,
    pf.dest_endpoint,
    pf.transport_protocol,
    f.application_protocol AS flow_app_protocol,
    f.packet_count AS flow_packet_count,
    f.bytes_sent + f.bytes_received AS flow_total_bytes,
    f.flow_state,
    datetime(f.start_time, 'unixepoch') AS flow_start,
    CASE 
      WHEN f.end_time IS NOT NULL THEN 
        datetime(f.end_time, 'unixepoch')
      ELSE 'ONGOING'
    END AS flow_end,
    CASE 
      WHEN f.end_time IS NOT NULL THEN 
        round(f.end_time - f.start_time, 3) || 's'
      ELSE 'ONGOING'
    END AS flow_duration
  FROM packet_flow pf
  LEFT JOIN flows f ON (
    pf.source_endpoint = f.src_ip || ':' || COALESCE(f.src_port, 0) AND
    pf.dest_endpoint = f.dst_ip || ':' || COALESCE(f.dst_port, 0) AND
    pf.transport_protocol = f.protocol
  )
)

-- MASTER QUERY OUTPUT
SELECT 
  -- Session and packet identification
  pf.session_name,
  pf.frame_number,
  pf.packet_time,
  
  -- Network flow context
  pf.source_endpoint || ' → ' || pf.dest_endpoint AS connection,
  pf.transport_protocol,
  pf.tcp_flags,
  
  -- Application layer
  COALESCE(pf.application_protocol, 'Unknown') AS app_protocol,
  pf.protocol_version,
  
  -- Data characteristics
  pf.packet_size || 'B' AS packet_size,
  CASE WHEN pf.payload_size > 0 THEN pf.payload_size || 'B' ELSE 'No payload' END AS payload_size,
  CASE WHEN pf.body_size > 0 THEN pf.body_size || 'B' ELSE 'No body' END AS body_size,
  
  -- Content analysis
  COALESCE(pf.content_type, 'Unknown') AS content_type,
  pf.content_preview,
  
  -- Security status
  CASE sa.max_severity_level
    WHEN 4 THEN '🔴 CRITICAL'
    WHEN 3 THEN '🟠 HIGH'
    WHEN 2 THEN '🟡 MEDIUM'
    WHEN 1 THEN '🔵 LOW'
    ELSE '✅ CLEAN'
  END AS security_status,
  
  COALESCE(sa.alert_count, 0) AS alerts,
  sa.security_alerts,
  
  -- Encryption and compression
  CASE WHEN pf.encryption_detected THEN '🔒 Encrypted' ELSE '📄 Plaintext' END AS encryption,
  COALESCE(pf.compression_type, 'None') AS compression,
  
  -- Protocol-specific details
  ps.http_details,
  ps.tls_details,
  ps.extracted_file_details,
  
  -- Flow context
  fc.flow_app_protocol,
  fc.flow_packet_count,
  fc.flow_total_bytes || 'B' AS flow_total_bytes,
  fc.flow_state,
  fc.flow_duration,
  
  -- Advanced indicators
  CASE 
    WHEN pf.vulnerabilities_detected != '[]' THEN '⚠️ Vulnerabilities'
    WHEN pf.suspicious_patterns IS NOT NULL THEN '👁️ Suspicious'
    WHEN sa.max_severity_level >= 3 THEN '🚨 Security Alert'
    WHEN pf.encryption_detected AND ps.tls_details IS NOT NULL THEN '🛡️ Secure'
    ELSE '📊 Normal'
  END AS analysis_summary,
  
  -- Hash for deduplication and reference
  substr(pf.content_hash, 1, 12) AS content_hash_short

FROM packet_flow pf
LEFT JOIN security_analysis sa USING (packet_id, transport_id, encoding_id, content_id)
LEFT JOIN protocol_specific ps USING (encoding_id)
LEFT JOIN flow_context fc USING (source_endpoint, dest_endpoint, transport_protocol)

ORDER BY 
  pf.session_name,
  pf.frame_number;

-- SUMMARY STATISTICS
-- Uncomment the following queries for additional insights:

/*
-- Session Summary
SELECT 
  'SESSION SUMMARY' as analysis_type,
  session_name,
  COUNT(*) as total_packets,
  COUNT(DISTINCT transport_protocol) as protocols_used,
  COUNT(DISTINCT application_protocol) as app_protocols,
  SUM(CASE WHEN security_status LIKE '%CRITICAL%' OR security_status LIKE '%HIGH%' THEN 1 ELSE 0 END) as high_risk_packets,
  AVG(packet_size) as avg_packet_size
FROM (previous_query_results)
GROUP BY session_name;

-- Protocol Distribution
SELECT 
  'PROTOCOL DISTRIBUTION' as analysis_type,
  transport_protocol,
  app_protocol,
  COUNT(*) as packet_count,
  AVG(packet_size) as avg_size,
  SUM(CASE WHEN encryption = '🔒 Encrypted' THEN 1 ELSE 0 END) as encrypted_packets
FROM (previous_query_results)
GROUP BY transport_protocol, app_protocol
ORDER BY packet_count DESC;

-- Security Alert Summary
SELECT 
  'SECURITY SUMMARY' as analysis_type,
  security_status,
  COUNT(*) as count,
  ROUND(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER(), 2) as percentage
FROM (previous_query_results)
GROUP BY security_status
ORDER BY count DESC;
*/
