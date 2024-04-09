// PR 472 from work
switch (placement) {
    case "top":
      arrowStyles.top = `${popoverRect.height + 8}px`;
      arrowStyles.left = `${popoverRect.width / 2}px`;
      arrowStyles.transform = "translateX(-50%) translateY(-100%) rotate(45deg)";
      break;
    case "top-start":
      arrowStyles.top = `${popoverRect.height + 8}px`;
      arrowStyles.left = `${popoverRect.width / 8}px`;
      arrowStyles.transform = "translateX(-50%) translateY(-100%) rotate(45deg)";
      break;
    case "top-end":
    arrowStyles.top = `${popoverRect.height + 8}px`;
    arrowStyles.left = `${(7 * popoverRect.width) / 8}px`;
    arrowStyles.transform = "translateX(-50%) translateY(-100%) rotate(45deg)";
    break;
    case "bottom":
      arrowStyles.bottom = `${popoverRect.height - 8}px`;
      arrowStyles.left = `${popoverRect.width / 2}px`;
      arrowStyles.transform = "translateX(-50%) translateY(0%) rotate(45deg)";
      break;
    case "bottom-start":
    arrowStyles.bottom = `${popoverRect.height - 8}px`;
    arrowStyles.left = `${popoverRect.width / 8}px`;
    arrowStyles.transform = "translateX(-50%) translateY(0%) rotate(45deg)";
    break;
    case "bottom-end":
      arrowStyles.bottom = `${popoverRect.height - 8}px`;
      arrowStyles.left = `${(7 * popoverRect.width) / 8}px`;
      arrowStyles.transform = "translateX(-50%) translateY(0%) rotate(45deg)";
      break;
    case "left":
      arrowStyles.left = `${popoverRect.width + 8}px`;
      arrowStyles.top = `${popoverRect.height / 2}px`;
      arrowStyles.transform = "translateX(-100%) translateY(-50%) rotate(45deg)";
      break;
    case "left-start":
    arrowStyles.left = `${popoverRect.width + 8}px`;
    arrowStyles.top = `${popoverRect.height / 4}px`;
    arrowStyles.transform = "translateX(-100%) translateY(-50%) rotate(45deg)";
    break;
    case "left-end":
    arrowStyles.left = `${popoverRect.width + 8}px`;
    arrowStyles.top = `${(3 * popoverRect.height) / 4}px`;
    arrowStyles.transform = "translateX(-100%) translateY(-50%) rotate(45deg)";
    break;
    case "right":
      arrowStyles.right = `${popoverRect.width - 8}px`;
      arrowStyles.top = `${popoverRect.height / 2}px`;
      arrowStyles.transform = "translateX(0%) translateY(-50%) rotate(45deg)";
      break;
    case "right-start":
    arrowStyles.right = `${popoverRect.width - 8}px`;
    arrowStyles.top = `${popoverRect.height / 4}px`;
    arrowStyles.transform = "translateX(0%) translateY(-50%) rotate(45deg)";
    break;
    case "right-end":
    arrowStyles.right = `${popoverRect.width - 8}px`;
    arrowStyles.top = `${(3 * popoverRect.height) / 4}px`;
    arrowStyles.transform = "translateX(0%) translateY(-50%) rotate(45deg)";
    break;
    default:
      break;
  }
