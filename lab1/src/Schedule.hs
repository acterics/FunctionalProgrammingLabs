module Schedule where

import Data.Time

data Schedule = Schedule{ schedule_id :: Integer, section_id :: Integer, day :: Integer, time_start :: TimeOfDay, time_end :: TimeOfDay }

