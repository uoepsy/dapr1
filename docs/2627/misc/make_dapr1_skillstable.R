library(tidyverse)
readxl::read_xlsx("DAPR1 skills development (1).xlsx")[1:40,] |> janitor::clean_names() |> 
  transmute(
    cat = factor(skill_category,
                 levels=c(
                   "Programming","Summarising","Visualising","Analysing","Inferring",
                   "Communicating","Reflecting"), ordered=T),
    block = block_introduced,
    skill = case_when(
      block == 1 & requires_human_grading!="N" ~ paste0(skill,"*<br><small>(marked after B1, B2, B3, B4)</small>"),
      block == 2 & requires_human_grading!="N" ~ paste0(skill,"*<br><small>(marked after B2, B3, B4)</small>"),
      block == 3 & requires_human_grading!="N" ~ paste0(skill,"*<br><small>(marked after B3, B4)</small>"),
      block == 4 & requires_human_grading!="N" ~ paste0(skill,"*<br><small>(marked after B4)</small>"),
      TRUE ~ skill
    ),
    stext = assessment_level_skill
  ) |> arrange(block, cat) -> df


sink("skilltable.html")


cat(
  '
  <!DOCTYPE html>
<html>
<head>
<style>
  :root {
    --primary-color: #0F4C81;
    --primary-hover: #0F4C81;
    --bg-color: #f8fafc;
    --card-bg: #ffffff;
    --text-main: #1e293b;
    --text-muted: #64748b;
    --border-color: #e2e8f0;
    --tooltip-bg: #0f172a;
  }
  body {
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background-color: var(--bg-color);
    color: var(--text-main);
    line-height: 1.5;
    margin: 0;
    padding: 20px;
  }
  .skill-desc{
    max-width: 1400px;
    margin: 0 auto;
  }
  button {
    background-color: var(--primary-color);
    color: white;
    border: none;
    padding: 10px 20px;
    font-size: 0.95rem;
    font-weight: 600;
    border-radius: 8px;
    cursor: pointer;
    box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
    transition: background-color 0.2s, transform 0.1s;
  }
  .table-wrapper {
    max-width: 1400px;
    margin: 0 auto;
    background: var(--card-bg);
    border-radius: 12px;
    box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.05), 0 4px 6px -2px rgba(0, 0, 0, 0.025);
    overflow-x: auto; /* Enables smooth horizontal scrolling on mobile */
    -webkit-overflow-scrolling: touch;
    border: 1px solid var(--border-color);
  }
  table { 
    border-collapse: collapse; 
    width: 100%; 
    min-width: 900px; /* Prevents columns from crushing together on smaller viewports */
  }
  th, td { 
    border: 1px solid #ddd; 
    padding: 15px; 
    text-align: center; 
    vertical-align: top; 
    width: 12.5%; 
    word-wrap: break-word; 
    background-color: #f1f5f9;
    color: var(--text-main);
    font-weight: 600;
    font-size: 0.8rem;
    letter-spacing: 0.05em;
  }
  td:first-child {
    font-weight: 600;
    color: var(--text-muted);
    background-color: #f8fafc;
    vertical-align: middle;
    width: 80px; 
  }
  .hover-trigger { 
    position: relative; 
    cursor: help; 
    color: var(--primary-color); 
    background-color: #e0e7ff;
    padding: 4px 8px;
    border-radius: 6px;
    font-weight: 600;
    font-size: 0.9rem;
    display: inline-block;
    margin: 6px 0;
    transition: background-color 0.2s;
  }
  .tooltip {
    visibility: hidden; width: 200px; background-color: #333; color: #fff;
    text-align: center; padding: 8px; border-radius: 6px; position: absolute;
    z-index: 1; bottom: 125%; left: 50%; margin-left: -100px;
    opacity: 0; transition: opacity 0.3s; font-weight: normal; font-size: 0.9em;
  }
  .hover-trigger:hover .tooltip { visibility: visible; opacity: 1; }

  body.show-all .tooltip {
    position: static; visibility: visible; opacity: 1; display: block;
    width: auto; background: none; color: #555; text-align: left;
    margin: 5px 0 10px 0; font-size: 0.9em; font-style: italic;
    border-top: 1px dotted #ccc; padding-top: 5px;
  }
</style>
</head>
<body>

<div style="text-align: center; margin: 20px;">
  <div class="skill-desc">
  <p>There are 40 skills to demonstrate across the year and they are all listed in this table. Once you’ve demonstrated a skill, it’s yours forever, and you can attempt a skill as many times as you like with no penalty for trying again (it’s your "best attempt", and not the most recent one, that indicates if you have demonstrated a skill).</p>
<p>Some skills are automatically graded: these are the multiple-choice style questions. These skills will be marked, and your demonstration of the skill will be updated, every time you attempt the skill.</p>
<p>Other skills require manual grading by somebody on the teaching team: these are the short-answer write-up questions. These skills are indicated by a *. We will mark these skills after every 5-week teaching block, so if you submit by the last Friday of each block, it will be marked in the subsequent weeks. You can submit a response to these questions any time you want as the semester goes on, or even submit multiple responses — at the end of each block we will grade the most recently submitted response.</p>
    <p><b>What about the exam?</b> The exam will not include any programming skills, because programming is typically a process trial and error, which cannot be captured in an exam. It will also not include any reflecting skills. Questions in the exam will cover 20 of the skills from the middle 5 columns of the table below.</p>
  </div>

  <button onclick="document.body.classList.toggle(\'show-all\')">Toggle Descriptions</button>
</div>
<div class="table-wrapper">
<table>
  <tr>
    <th></th>
    <th>Programming<br><small>(not in the exam)</small></th>
    <th>Summarising</th>
    <th>Visualising</th>
    <th>Analysing</th>
    <th>Inferring</th>
    <th>Communicating</th>
    <th>Reflecting<br><small>(not in the exam)</small></th>
  </tr>
  
  '
)











for(i in 1:4){
  cat(paste0("
  <tr>\n
    <td>Block ",i,"</td>\n"))
  for(cc in levels(df$cat)){
    cat("<td>\n")
    if(length(df$skill[df$block==i & df$cat==cc])>0){
      cat(paste0(
        '<span class="hover-trigger">',
        df$skill[df$block==i & df$cat==cc],
        '<span class="tooltip">',
        df$stext[df$block==i & df$cat==cc],
        '</span></span><br>\n'
      ))
    }
    cat('</td>\n')
  }
  cat('</tr>\n')
}

cat('</table>\n
    </div>\n
  </body>\n
</html>')


sink()

