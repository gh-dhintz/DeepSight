# Getting Started with Foam

Foam is a personal knowledge management and sharing system inspired by Roam Research, built on Visual Studio Code and GitHub. It's designed for organizing your thoughts, notes, and research in a connected, networked way.

## What is Foam?

Foam combines the power of Visual Studio Code with GitHub to create a lightweight, extensible, and version-controlled knowledge base. Unlike traditional note-taking apps, Foam focuses on creating connections between your ideas through bidirectional linking.

## Installation

### Prerequisites
- **Visual Studio Code** - Download from [code.visualstudio.com](https://code.visualstudio.com/)
- **Git** - Download from [git-scm.com](https://git-scm.com/)
- **GitHub account** (optional but recommended)

### Setup Methods

#### Option 1: Use the Foam Template (Recommended)
1. Visit the [Foam template repository](https://github.com/foambubble/foam-template)
2. Click "Use this template" → "Create a new repository"
3. Name your repository (e.g., "my-foam-kb")
4. Clone the repository to your local machine
5. Open the folder in VS Code

#### Option 2: Manual Setup
1. Create a new folder for your Foam workspace
2. Open it in VS Code
3. Install the Foam extension pack from the VS Code marketplace
4. Initialize with `Foam: Create New Note` command

## Essential Extensions

Foam works best with these VS Code extensions (automatically included with Foam extension pack):

- **Foam for VSCode** - Core Foam functionality
- **Markdown All in One** - Enhanced markdown support
- **Markdown Links** - Link management
- **GitLens** - Git integration and history
- **Spell Right** - Spell checking

## Basic Concepts

### Notes and Markdown
- All notes are written in Markdown (`.md` files)
- Use standard markdown syntax for formatting
- Each note should focus on a single concept or idea

### Wikilinks
Create connections between notes using double brackets:
```markdown
This connects to [[Another Note]]
This creates a new note: [[New Concept]]
```

### Tags
Organize notes with hashtags:
```markdown
#project #idea #todo
```

### Daily Notes
Create dated notes for journaling and tracking:
```markdown
# 2024-06-13
Today I learned about [[Foam Setup]]
```

## Your First Foam Workspace

### Folder Structure
```
my-foam-kb/
├── .foam/           # Foam configuration
├── .vscode/         # VS Code settings
├── docs/            # Published notes (optional)
├── templates/       # Note templates
├── inbox/           # Unprocessed notes
├── journal/         # Daily notes
├── projects/        # Project-specific notes
└── README.md        # Workspace overview
```

### Creating Your First Note
1. Use `Ctrl+Shift+P` (or `Cmd+Shift+P` on Mac)
2. Type "Foam: Create New Note"
3. Enter a title for your note
4. Start writing in Markdown

### Linking Notes
```markdown
# My First Note

This is connected to [[My Second Note]].

I'm also interested in [[Machine Learning]] and [[Personal Productivity]].

Tags: #getting-started #foam
```

## Key Features

### Graph Visualization
- View your knowledge graph with `Foam: Show Graph`
- See connections between all your notes
- Identify clusters and gaps in your knowledge

### Backlinks
- See which notes link to the current note
- Access via the "Foam: Show Backlinks" command
- Helps discover unexpected connections

### Orphaned Notes
- Find notes without any links
- Use `Foam: Find Orphaned Notes` command
- Helps maintain a connected knowledge base

### Note Templates
Create templates for consistent note structure:
```markdown
# Template: Daily Note
Date: {{date}}
Weather: 
Mood: 

## What I learned today
- 

## Tomorrow's priorities
- 

## Thoughts and reflections

```

## Daily Workflow

### Morning Routine
1. Open daily note (`Ctrl+Shift+P` → "Foam: Open Daily Note")
2. Review yesterday's note and carry over unfinished items
3. Set intentions for the day

### Throughout the Day
1. Capture quick thoughts in inbox notes
2. Link new concepts to existing notes
3. Use tags to categorize information
4. Create dedicated notes for substantial ideas

### Evening Review
1. Process inbox notes
2. Strengthen connections between related notes
3. Update project notes with progress
4. Plan tomorrow's priorities

## Best Practices

### Note-Taking
- **Atomic notes**: One concept per note
- **Descriptive titles**: Make them searchable and memorable
- **Link liberally**: Create connections even if they seem obvious
- **Use your own words**: Don't just copy-paste; rephrase and reflect

### Organization
- Start simple, let structure emerge naturally
- Use folders sparingly; rely more on links and tags
- Regularly review and refactor your notes
- Keep an inbox for quick captures

### Linking Strategy
- Link to concepts, not just topics
- Create hub notes for major subjects
- Use consistent naming conventions
- Don't be afraid to create placeholder notes

## Advanced Features

### Publishing
- Use GitHub Pages to publish your notes online
- Configure in `.vscode/settings.json`
- Great for sharing knowledge publicly

### Custom CSS
Customize appearance with CSS:
```css
/* .vscode/foam.css */
.foam-note-link {
    color: #0066cc;
    text-decoration: none;
}
```

### Workspace Settings
Configure Foam behavior in `.vscode/settings.json`:
```json
{
    "foam.edit.linkReferenceDefinitions": "off",
    "foam.openDailyNote.directory": "journal",
    "foam.openDailyNote.titleFormat": "yyyy-mm-dd"
}
```

## Common Commands

- `Ctrl+Shift+P` - Open command palette
- `Foam: Create New Note` - Create a new note
- `Foam: Open Daily Note` - Open today's journal entry
- `Foam: Show Graph` - Visualize your knowledge graph
- `Foam: Find Orphaned Notes` - Find unlinked notes
- `Foam: Generate References` - Update link references

## Tips for Success

1. **Start small**: Begin with a few notes and grow organically
2. **Be consistent**: Develop daily habits around note-taking
3. **Link as you write**: Don't defer linking to later
4. **Review regularly**: Spend time exploring your graph and making new connections
5. **Don't over-organize**: Let the network structure emerge naturally
6. **Use templates**: Create consistent formats for different note types
7. **Backup regularly**: Commit changes to Git frequently

## Troubleshooting

### Common Issues
- **Links not working**: Check for typos in double brackets
- **Graph not updating**: Reload VS Code window
- **Extensions not loading**: Reinstall Foam extension pack
- **Sync issues**: Ensure Git is properly configured

### Getting Help
- [Foam Documentation](https://foambubble.github.io/foam/)
- [GitHub Discussions](https://github.com/foambubble/foam/discussions)
- [Discord Community](https://discord.gg/HV2tn2FpEk)

## Next Steps

Once you're comfortable with the basics:

1. Explore advanced linking patterns
2. Set up automated publishing with GitHub Actions
3. Create custom templates for different note types
4. Integrate with other tools in your workflow
5. Contribute to the Foam community

Remember: The goal isn't to create the perfect system, but to build a sustainable practice of connected thinking. Start simple, be consistent, and let your knowledge graph grow naturally over time.