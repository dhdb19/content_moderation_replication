copy
  (select created_at,
          platform_name,
          category,
          category_addition,
          category_specification,
          category_specification_other,
          automated_decision,
          decision_ground,
          incompatible_content_ground,
          incompatible_content_explanation,
          illegal_content_explanation,
          illegal_content_legal_ground
   from read_parquet('./eu-dsa-tdb-sor-full-social-media/year=$y/month=$m/day=$d/*.parquet.zst',
                     hive_partitioning = false)
   where list_contains(['Facebook', 'Instagram', 'LinkedIn', 'Pinterest', 'Reddit', 'Snapchat',
                        'Threads', 'TikTok', 'X', 'YouTube', 'Amazon Store'], platform_name)
     or (platform_name = 'Google Shopping'
         and random() < .01))
    to 'eu_tdb/$y-$m-$d.parquet'
     (format 'parquet',
      row_group_size 4194304);
