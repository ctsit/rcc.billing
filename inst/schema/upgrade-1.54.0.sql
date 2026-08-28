-- upgrade for release 1.54.0
alter table service_type
  add column `start_date` datetime DEFAULT NULL AFTER `billing_frequency`
;

update service_type set start_date = '2023-11-01' where start_date is null;

insert into service_type (service_type_code, service_type, price, billing_frequency, start_date)
values
  (1, 'Annual REDCap Project Maintenance', 150, 12, '2026-10-01'),
  -- service_type is not authoritative for this rate (not read by any script);
  -- recorded here for completeness only.
  (2, 'REDCap consulting', 150, 0, '2026-10-01');
